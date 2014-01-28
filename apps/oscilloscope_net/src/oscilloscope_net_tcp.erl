-module(oscilloscope_net_tcp).
-export([start_link/4, init/4]).

start_link(UserID, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [UserID, Socket, Transport, Opts]),
    {ok, Pid}.

init(UserID, Socket, Transport, Opts) ->
    folsom_metrics:notify({oscilloscope_net, inits, Transport}, {inc, 1}),
    ok = ranch:accept_ack(UserID),
    Parser = proplists:get_value(parser, Opts),
    loop(Socket, Transport, Parser, UserID).

loop(Socket, Transport, Parse, UserID) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Data} ->
            folsom_metrics:notify({oscilloscope_net, recvs, tcp}, {inc, 1}),
            folsom_metrics:notify(
                {oscilloscope_net, recvs, Transport}, {inc, 1}
            ),
            lists:foreach(
                fun({Metric, Value, Timestamp}) ->
                    oscilloscope_cache:process(UserID, Metric, <<"">>, Timestamp, Value)
                end,
                Parse(Data)
            ),
            loop(Socket, Transport, Parse, UserID);
        Error ->
            lager:error(
                "Got error ~p when attempting to receive from socket ~p",
                [Error, Socket]
            ),
            ok = Transport:close(Socket)
    end.
