-module(oscilloscope_net_user).

-export([start/2]).

start(UserID, Port) ->
    {ok, Pid} = ranch:start_listener(
        UserID,
        1,
        ranch_tcp,
        [{port, Port}],
        oscilloscope_net_tcp,
        [{parser, fun oscilloscope_net_protocols:graphite/1}]
    ),
    {ok, Pid}.
