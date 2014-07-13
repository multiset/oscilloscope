-module(oscilloscope_entities).

-export([
    start/0,
    stop/0
]).

start() ->
    application:start(oscilloscope_entities).

stop() ->
    ok.
