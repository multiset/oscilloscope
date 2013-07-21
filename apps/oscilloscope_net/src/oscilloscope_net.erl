-module(oscilloscope_net).

-export([start/0, stop/0]).

start() ->
    application:start(ranch),
    application:start(oscilloscope_net).

stop() ->
    ok.
