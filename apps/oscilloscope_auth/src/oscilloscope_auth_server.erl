-module(oscilloscope_auth_server).

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

-include_lib("oscilloscope_auth/include/oscilloscope_auth.hrl").

-record(state, {
    rules,
    metrics
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, _, Orgs} = oscilloscope_metadata_sql:named(get_orgs, []),
    lists:foldl(fun({OrgID, OrgName}, _) ->
        ets:insert(orgs, #org{id=OrgID, name=OrgName})
    end, ok, Orgs),
    {ok, _, Metrics} = oscilloscope_metadata_sql:named(get_metrics, []),
    MetricDict = lists:foldl(fun
        ({OrgID, TeamID, Tags, MetricID}, Dict) ->
            dict:append({OrgID, TeamID}, {Tags, MetricID}, Dict)
    end, dict:new(), Metrics),
    {ok, #state{metrics=MetricDict}}.


handle_call({grant_perms, OrgID, TeamID, Tags, Level}, _From, State) ->
    #state{rules=Rules0} = State,
    Rules = dict:append({OrgID, TeamID}, {Tags, Level}, Rules0),
    {reply, ok, #state{rules=Rules}};


handle_call({get_metrics, OrgID, UserID, Tags}, _From, State) ->
    TeamIDs = ets:lookup(user_org_teams, [{OrgID, UserID}]),
    {reply, get_metrics(OrgID, TeamIDs, Tags, State), State}.


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


get_authed_metrics(OrgID, TeamIDs, State) ->
    #state{metrics=AllMetrics, rules=AllRules} = State,
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

get_metrics(OrgID, TeamIDs, Tags, State) ->
    Authed = get_authed_metrics(OrgID, TeamIDs, State),
    filter_metrics(Tags, Authed).


handle_cast({create_metric, OwnerID, MetricID, Props}, State) ->
    #state{metrics=Metrics0} = State,
    Metrics = dict:append(OwnerID, {Props, MetricID}, Metrics0),
    {noreply, State#state{metrics=Metrics}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
