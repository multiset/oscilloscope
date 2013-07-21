-module(oscilloscope_net_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = ranch:start_listener(
        graphite,
        1,
        ranch_tcp,
        [{port, 2003}],
        oscilloscope_net_tcp,
        [{parser, fun oscilloscope_net_protocols:graphite/1}]
    ),
    oscilloscope_net_sup:start_link().

stop(_State) ->
    ok.
