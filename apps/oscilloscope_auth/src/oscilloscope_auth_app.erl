-module(oscilloscope_auth_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    ets:new(user_org_teams, [named_table, bag]),
    {ok, _, Resp} = oscilloscope_metadata:named(get_user_org_teams, []),
    lists:foldl(fun({UserID, OrgID, TeamID}, _) ->
        ets:insert(user_org_teams, {{OrgID, UserID}, TeamID})
    end, ok, Resp).

stop(_State) ->
    ok.
