-module(osc_entities_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("osc_entities.hrl").


start(_StartType, _StartArgs) ->
    osc_entities_sup:start_link().

stop(_State) ->
    ok.
