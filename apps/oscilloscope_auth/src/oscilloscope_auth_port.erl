-module(oscilloscope_auth_port).

-export([start/2]).

-include("oscilloscope_auth.hrl").

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
