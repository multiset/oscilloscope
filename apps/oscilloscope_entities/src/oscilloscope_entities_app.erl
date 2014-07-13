-module(oscilloscope_entities_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("oscilloscope_entities.hrl").


start(_StartType, _StartArgs) ->
    oscilloscope_entities_sup:start_link().

stop(_State) ->
    ok.
