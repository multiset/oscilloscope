-module(osc_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    osc_sup:start_link().

stop(_State) ->
    ok.
