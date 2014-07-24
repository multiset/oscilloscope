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
    ets:new(user_org_teams, [named_table, public]),
    ets:new(users, [named_table, {keypos, #user.name}, public]),
    ets:new(user_names, [named_table, public]),
    {ok, _, RawEmails} = oscilloscope_metadata_sql:named(select_emails, []),
    Emails = lists:foldl(fun({UserID, Email}, Dict) ->
        dict:append(UserID, Email, Dict)
    end, dict:new(), RawEmails),
    {ok, _, Users} = oscilloscope_metadata_sql:named(select_users, []),
    lists:foldl(fun({UserID, OwnerID, Name, Password}, _) ->
        UserEmails = case dict:find(UserID, Emails) of
            {ok, UserEmails0} -> UserEmails0;
            error -> []
        end,
        User = #user{
            id=UserID,
            owner_id=OwnerID,
            name=Name,
            emails=UserEmails,
            password=Password,
            orgs=dict:new()
        },
        true = ets:insert(users, User),
        true = ets:insert(user_names, {UserID, Name})
    end, ok, Users),

    ets:new(orgs, [named_table, public, {keypos, #org.name}]),
    ets:new(org_names, [named_table, public]),
    {ok, _, Orgs} = oscilloscope_metadata_sql:named(get_orgs, []),
    lists:foldl(fun({{OrgID, OwnerID, OrgName}}, _) ->
        Org = #org{
            id=OrgID,
            owner_id=OwnerID,
            name=OrgName,
            members=[]
        },
        ets:insert(orgs, Org),
        ets:insert(org_names, {OrgID, OrgName})
    end, ok, Orgs),

    ets:new(teams, [named_table, public, {keypos, #team.key}]),
    ets:new(team_names, [named_table, public]),
    {ok, _, AllTeams} = oscilloscope_metadata_sql:named(get_teams, []),
    lists:foldl(fun({{OrgID, TeamID, TeamName}}, _) ->
        Team = #team{
            id=TeamID,
            key={OrgID, TeamName},
            members=[]
        },
        ets:insert(teams, Team),
        ets:insert(team_names, {{OrgID, TeamID}, {OrgID, TeamName}})
    end, ok, AllTeams),

    % Populate orgs field in user record of users ets table
    {ok, _, UserTeams} = oscilloscope_metadata_sql:named(get_all_user_teams, []),
    {LastUserID, LastTeam, AggregatedUTs0} = lists:foldl(fun(IDs, Acc) ->
        {UserID, OrgID, TeamID} = IDs,
        case Acc of
            {UserID, Teams, TotalAcc} ->
                {UserID, dict:append(OrgID, TeamID, Teams), TotalAcc};
            {PrevUserID, Teams, TotalAcc} ->
                NewTeams = dict:append(OrgID, TeamID, dict:new()),
                case {PrevUserID, Teams} of
                    {undefined, undefined} ->
                        {UserID, NewTeams, TotalAcc};
                    _ ->
                        {UserID, NewTeams, [{PrevUserID, Teams}|TotalAcc]}
                end
        end
    end, {undefined, undefined, []}, UserTeams),

    AggregatedUTs = case LastUserID of
        undefined -> AggregatedUTs0;
        _ -> [{LastUserID, LastTeam}|AggregatedUTs0]
    end,


    lists:foldl(fun({UserID, Teams}, _) ->
        [{_,UserName}] = ets:lookup(user_names, UserID),
        [User] = ets:lookup(users, UserName),
        ets:insert(users, User#user{orgs=Teams})
    end, ok, AggregatedUTs),

    % Populate users, orgs, and teams ets table with membership info
    {ok, _, OrgTeams} = oscilloscope_metadata_sql:named(get_all_org_teams, []),
    populate_ets_records(OrgTeams, org_names, orgs, #org.teams),
    {ok, _, OrgMembers} = oscilloscope_metadata_sql:named(get_all_org_members, []),
    populate_ets_records(OrgMembers, org_names, orgs, #org.members),
    {ok, _, TeamMembers0} = oscilloscope_metadata_sql:named(get_all_team_members, []),
    TeamMembers = lists:map(fun({OrgID, TeamID, UserID}) ->
        {{OrgID, TeamID}, UserID}
    end, TeamMembers0),
    populate_ets_records(TeamMembers, team_names, teams, #team.members),

    {ok, _, OrgMetrics} = oscilloscope_metadata_sql:named(get_org_metrics, []),
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

populate_ets_records(KVs, IDToNameTab, RecordTab, RecordElement) ->
    AggregatedKVs = lists:foldl(fun({Key, Value}, Dict) ->
        dict:append(Key, Value, Dict)
    end, dict:new(), KVs),
    dict:fold(fun(Key, Values, _) ->
        [{_,Name}] = ets:lookup(IDToNameTab, Key),
        [Record] = ets:lookup(RecordTab, Name),
        ets:insert(RecordTab, setelement(RecordElement, Record, Values))
    end, ok, AggregatedKVs).


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
