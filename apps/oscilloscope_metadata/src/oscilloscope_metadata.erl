-module(oscilloscope_metadata).

-export([start/0, stop/0]).

start() ->
    application:start(oscilloscope_metadata).

stop() ->
    application:stop(oscilloscope_metadata).
