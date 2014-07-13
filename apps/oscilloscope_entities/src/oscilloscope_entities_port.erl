-module(oscilloscope_entities_port).

-export([start/2, create/1, create/2]).

-include("oscilloscope_entities.hrl").

-define(PORT_SERVER, oscilloscope_entities_port_server).

-spec start(integer(), integer()) -> ok | {error, binary()}.
start(OwnerID, Port) ->
    {ok, Pid} = ranch:start_listener(
        OwnerID,
        1,
        ranch_tcp,
        [{port, Port}],
        oscilloscope_net_tcp,
        [{parser, fun oscilloscope_net_protocols:graphite/1}]
    ),
    {ok, Pid}.

-spec create(integer()) -> ok | {error, binary()}.
create(OwnerID) ->
    gen_server:call(?PORT_SERVER, {create_port, OwnerID}).

-spec create(integer(), integer()) -> ok | {error, binary()}.
create(OwnerID, Port) ->
    gen_server:call(?PORT_SERVER, {create_port, OwnerID, Port}).
