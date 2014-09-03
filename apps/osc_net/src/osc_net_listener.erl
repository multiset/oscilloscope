-module(osc_net_listener).

-export([start/2]).

start(OwnerId, Port) ->
    {ok, Pid} = ranch:start_listener(
        OwnerId,
        10,
        ranch_tcp,
        [{port, Port}],
        osc_net_tcp,
        [{parser, fun osc_net_protocols:graphite/2}]
    ),
    {ok, Pid}.
