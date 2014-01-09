-module(oscilloscope_http_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("oscilloscope_http.hrl").

start(_StartType, _StartArgs) ->
    {ok, _, Users} = oscilloscope_sql:named(all_users, []),
    ets:new(user_cache, [named_table, public, {keypos, #user.name}]),
    lists:map(fun({Name, Password, Port, Id}) ->
        ets:insert(user_cache, #user{name=Name, password=Password, port=Port, id=Id})
    end, Users),
    oscilloscope_http_sup:start_link().

stop(_State) ->
    ok.
