-module(oscilloscope_net_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    oscilloscope_net_graphite:start(),
    oscilloscope_net_sup:start_link().

stop(_State) ->
    ok.
