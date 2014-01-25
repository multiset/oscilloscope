-module(oscilloscope_net_app).
-behaviour(application).

-export([start/2, stop/1]).

-include_lib("oscilloscope_http/include/oscilloscope_http.hrl").

start(_StartType, _StartArgs) ->
    ets:foldl(fun(#user{id=UserID, port=Port}, _) ->
        oscilloscope_net_user:start(UserID, Port)
    end, nil, user_cache),
    oscilloscope_net_sup:start_link().

stop(_State) ->
    ok.
