-module(oscilloscope_auth).

-export([
    start/0,
    stop/0
]).

start() ->
    application:start(oscilloscope_auth).

stop() ->
    ok.
