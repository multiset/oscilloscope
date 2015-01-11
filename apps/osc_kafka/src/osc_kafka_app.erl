-module(osc_kafka_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    osc_kafka_sup:start_link().


stop(_State) ->
    ok.
