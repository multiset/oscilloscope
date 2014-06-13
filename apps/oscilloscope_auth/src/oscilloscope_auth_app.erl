-module(oscilloscope_auth_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("oscilloscope_auth.hrl").


start(_StartType, _StartArgs) ->
    ets:new(user_org_teams, [named_table, bag]),
    ets:new(user_cache, [named_table, {keypos, #user.id}]),
    {ok, _, Resp} = oscilloscope_metadata_sql:named(get_user_org_teams, []),
    lists:foldl(fun({UserID, OrgID, TeamID}, _) ->
        ets:insert(user_org_teams, {{OrgID, UserID}, TeamID})
    end, ok, Resp),
    oscilloscope_auth_sup:start_link().

stop(_State) ->
    ok.
