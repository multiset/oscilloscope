-module(osc_kafka).

-export([
    start/0,
    stop/0
]).

start() ->
    application:start(osc_kafka).

stop() ->
    application:stop(osc_kafka).
