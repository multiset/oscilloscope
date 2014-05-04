-module(oscilloscope_net_tcp).
-export([start_link/4, init/4]).

-record(state, {
    owner_id,
    buffer
}).

start_link(OwnerId, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [OwnerId, Socket, Transport, Opts]),
    {ok, Pid}.

init(OwnerId, Socket, Transport, Opts) ->
    folsom_metrics:notify({oscilloscope_net, inits, Transport}, {inc, 1}),
    ok = ranch:accept_ack(OwnerId),
    Parser = proplists:get_value(parser, Opts),
    Buffer = <<"">>,
    loop(Socket, Transport, Parser, #state{owner_id=OwnerId, buffer=Buffer}).

loop(Socket, Transport, Parse, State) ->
    #state{owner_id=OwnerId, buffer=PrevBuffer} = State,
    case Transport:recv(Socket, 0, 600000) of
        {ok, Data0} ->
            folsom_metrics:notify({oscilloscope_net, recvs, tcp}, {inc, 1}),
            folsom_metrics:notify(
                {oscilloscope_net, recvs, Transport}, {inc, 1}
            ),
            {Data, Buffer} = Parse(Data0, PrevBuffer),
            lists:foreach(
                fun({Metric, Timestamp, Value}) ->
                    oscilloscope_cache:process(
                        {OwnerId, Metric},
                        Timestamp,
                        Value
                    )
                end,
                Data
            ),
            loop(Socket, Transport, Parse, State#state{buffer=Buffer});
        Error ->
            lager:error(
                "Got error ~p when attempting to receive from socket ~p",
                [Error, Socket]
            ),
            ok = Transport:close(Socket)
    end.
