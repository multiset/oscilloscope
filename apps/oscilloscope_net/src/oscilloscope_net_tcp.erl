-module(oscilloscope_net_tcp).
-export([start_link/3, init/3]).

-record(state, {
    buffer
}).

start_link(Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Socket, Transport, Opts]),
    {ok, Pid}.

init(Socket, Transport, Opts) ->
    folsom_metrics:notify({oscilloscope_net, inits, Transport}, {inc, 1}),
    ok = ranch:accept_ack(),
    Parser = proplists:get_value(parser, Opts),
    loop(Socket, Transport, Parser, #state{buffer = <<"">>}).

loop(Socket, Transport, Parse, State) ->
    #state{buffer=PrevBuffer} = State,
    case Transport:recv(Socket, 0, 600000) of
        {ok, Data0} ->
            folsom_metrics:notify({oscilloscope_net, recvs, tcp}, {inc, 1}),
            folsom_metrics:notify(
                {oscilloscope_net, recvs, Transport}, {inc, 1}
            ),
            {Data, Buffer} = Parse(Data0, PrevBuffer),
            lists:foreach(
                fun({Metric, Host, Timestamp, Value}) ->
                    oscilloscope_cache:process(Metric, Host, Timestamp, Value)
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
