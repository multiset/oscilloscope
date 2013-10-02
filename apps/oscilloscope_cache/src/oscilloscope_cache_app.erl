-module(oscilloscope_cache_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    oscilloscope_cache_sup:start_link().

stop(_State) ->
    ok.
