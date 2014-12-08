-module(osc_meta_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    osc_http:load_routes(osc_meta),
    osc_meta_sup:start_link().

stop(_State) ->
    ok.
