-module(osc_http).

-export([
    start/0,
    stop/0
]).

start() ->
    application:start(osc_http).

stop() ->
    ok.
