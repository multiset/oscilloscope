-module(osc_net).

-export([start/0, stop/0]).

start() ->
    application:start(ranch),
    application:start(osc_net).

stop() ->
    ok.
