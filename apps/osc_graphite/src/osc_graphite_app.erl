-module(osc_graphite_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    osc_http:load_routes(osc_graphite),
    osc_graphite_sup:start_link().

stop(_State) ->
    ok.
