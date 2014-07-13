-module(oscilloscope_entities_server).

-behaviour(gen_server).

-export([start_link/0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("oscilloscope_entities.hrl").

-record(state, {
    rules,
    org_metrics,
    user_metrics
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(user_org_teams, [named_table, bag, public]),
    ets:new(user_cache, [named_table, {keypos, #user.id}, public]),
    {ok, _, Users} = oscilloscope_metadata_sql:named(select_users, []),
    lists:foldl(fun({UserID, OwnerID, Name, Email, Password}, _) ->
        User = #user{
            id=UserID,
            owner_id=OwnerID,
            name=Name,
            email=Email,
            password=Password
        },
        true = ets:insert(user_cache, User)
    end, ok, Users),

    ets:new(orgs, [named_table, bag, public, {keypos, #org.id}]),
    ets:new(org_ids, [named_table, bag, public]),
    {ok, _, Orgs} = oscilloscope_metadata_sql:named(get_orgs, []),
    lager:error("oscilloscope_metadata: get_orgs: ~p", [Orgs]),
    lists:foldl(fun({{OrgID, OrgName}}, _) ->
        ets:insert(orgs, #org{id=OrgID, name=OrgName}),
        ets:insert(org_ids, {OrgName, OrgID})
    end, ok, Orgs),

    ets:new(teams, [named_table, bag, public, {keypos, #org.id}]),
    ets:new(team_ids, [named_table, bag, public]),
    {ok, _, Teams} = oscilloscope_metadata_sql:named(get_teams, []),
    lager:error("oscilloscope_metadata: get_teams: ~p", [Teams]),
    lists:foldl(fun({{OrgID, TeamID, TeamName}}, _) ->
        ets:insert(teams, #team{id=TeamID, org_id=OrgID, name=TeamName}),
        ets:insert(team_ids, {{OrgID, TeamName}, TeamID})
    end, ok, Teams),

    ets:new(org_members, [named_table, public]),
    {ok, _, OrgMembers} = oscilloscope_metadata_sql:named(get_org_members, []),
    lager:error("OrgMembers: ~p", [OrgMembers]),
    lists:foldl(fun({OrgID, UserID}, _) ->
        ets:insert(org_members, {OrgID, UserID})
    end, ok, OrgMembers),

    ets:new(team_members, [named_table, public]),
    {ok, _, TeamMembers} = oscilloscope_metadata_sql:named(get_team_members, []),
    lager:error("TeamMembers: ~p", [TeamMembers]),
    lists:foldl(fun({OrgID, TeamID, UserID}, _) ->
        ets:insert(team_members, {OrgID, TeamID, UserID})
    end, ok, TeamMembers),

    {ok, _, OrgMetrics} = oscilloscope_metadata_sql:named(get_org_metrics, []),
    lager:error("OrgMetrics: ~p", [OrgMetrics]),
    OrgMetricDict = lists:foldl(fun
        ({OrgID, TeamID, TagNames, TagValues, MetricID}, Dict) ->
            Tags = lists:zip(TagNames, TagValues),
            dict:append({OrgID, TeamID}, {Tags, MetricID}, Dict)
    end, dict:new(), OrgMetrics),

    {ok, _, UserMetrics} = oscilloscope_metadata_sql:named(get_user_metrics, []),

    State = #state{
        org_metrics=OrgMetricDict,
        user_metrics=UserMetrics
    },
    {ok, State}.


handle_call({grant_perms, OrgID, TeamID, Tags, Level}, _From, State) ->
    #state{rules=Rules0} = State,
    Rules = dict:append({OrgID, TeamID}, {Tags, Level}, Rules0),
    {reply, ok, #state{rules=Rules}};

handle_call({find_org_metrics, OrgID, UserID, Tags}, _From, State) ->
    TeamIDs = ets:lookup(user_org_teams, [{OrgID, UserID}]),
    {reply, find_org_metrics(OrgID, TeamIDs, Tags, State), State};

handle_call({find_user_metrics, UserID, Tags}, _From, State) ->
    {reply, find_user_metrics(UserID, Tags, State), State}.


filter_metrics(MatchPairs, MetricList) ->
    lists:filter(fun({Metric, _MID, _AuthLevel}) ->
        lists:all(fun({Key, Regex}) ->
            case lists:keyfind(Key, 1, Metric) of
                false ->
                    false;
                {_, Value} ->
                    re_match(Value, Regex)
            end
        end, MatchPairs)
    end, MetricList).


re_match(Subject, Regex) ->
    case re:run(Subject, Regex) of
        {match, _} -> true;
        _ -> false
    end.


find_authed_org_metrics(OrgID, TeamIDs, State) ->
    #state{org_metrics=AllMetrics, rules=AllRules} = State,
    % Get all applicable rules
    Rules = lists:flatmap(fun(TeamID) ->
        case dict:find({OrgID, TeamID}, AllRules) of
            error -> [];
            {ok, Rules} -> Rules
        end
    end, TeamIDs),
    lists:foldl(fun(TeamID) ->
        case dict:find({OrgID, TeamID}, AllMetrics) of
            {ok, Metrics} ->
                lists:filtermap(fun({Metric, MID}) ->
                    FinalLevel = lists:foldl(fun({Rule, AuthLevel}, CurrentLevel) ->
                        IsAuthed = lists:foldl(fun({Key, Value}, Acc0) ->
                            case dict:find(Key, Rule) of
                                error ->
                                    Acc0;
                                {ok, Regex} ->
                                    re_match(Value, Regex)
                            end
                        end, false, Metric),
                        if IsAuthed ->
                            CurrentLevel bor AuthLevel;
                        true ->
                            CurrentLevel
                        end
                    end, 0, Rules),

                    if FinalLevel > 0 ->
                        {true, {Metric, MID, FinalLevel}};
                    true ->
                        false
                    end
                end, [], Metrics);
            error ->
                []
        end
    end, [], TeamIDs).

find_org_metrics(OrgID, TeamIDs, Tags, State) ->
    Authed = find_authed_org_metrics(OrgID, TeamIDs, State),
    filter_metrics(Tags, Authed).

find_user_metrics(UserID, Tags, State) ->
    case dict:find(UserID, State#state.user_metrics) of
        {ok, Metrics} ->
            filter_metrics(Tags, Metrics);
        error ->
            not_found
    end.

handle_cast({create_metric, OwnerID, MetricID, Props}, State) ->
    #state{org_metrics=Metrics0} = State,
    Metrics = dict:append(OwnerID, {Props, MetricID}, Metrics0),
    {noreply, State#state{org_metrics=Metrics}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
