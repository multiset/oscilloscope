-module(osc_entities).

-export([
    start/0,
    stop/0
]).

start() ->
    application:start(osc_entities).

stop() ->
    ok.
