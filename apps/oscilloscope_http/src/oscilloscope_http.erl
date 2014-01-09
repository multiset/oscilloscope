-module(oscilloscope_http).

-export([
    start/0,
    stop/0
]).

start() ->
    application:start(oscilloscope_http).

stop() ->
    ok.
