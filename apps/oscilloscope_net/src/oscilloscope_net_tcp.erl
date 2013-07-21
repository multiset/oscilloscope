-module(oscilloscope_net_tcp).
-export([start_link/4, init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, Opts) ->
    ok = ranch:accept_ack(Ref),
    Parser = proplists:get_value(parser, Opts),
    loop(Socket, Transport, Parser).

loop(Socket, Transport, Parse) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Data} ->
            Parsed = Parse(Data),
            [oscilloscope_cache:process(P) || P <- Parsed],
            loop(Socket, Transport, Parse);
        _ ->
            ok = Transport:close(Socket)
    end.
