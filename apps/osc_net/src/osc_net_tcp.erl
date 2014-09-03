-module(osc_net_tcp).

-compile([{parse_transform, lager_transform}]).

-export([start_link/4, init/4]).

-record(state, {
    owner_id,
    buffer
}).

start_link(OwnerId, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [OwnerId, Socket, Transport, Opts]),
    {ok, Pid}.

init(OwnerId, Socket, Transport, Opts) ->
    folsom_metrics:notify({osc_net, inits, Transport}, {inc, 1}),
    ok = ranch:accept_ack(OwnerId),
    Parser = proplists:get_value(parser, Opts),
    Buffer = <<"">>,
    loop(Socket, Transport, Parser, #state{owner_id=OwnerId, buffer=Buffer}).

loop(Socket, Transport, Parse, State) ->
    #state{owner_id=OwnerID, buffer=PrevBuffer} = State,
    case Transport:recv(Socket, 0, 600000) of
        {ok, Data0} ->
            folsom_metrics:notify({osc_net, recvs, tcp}, {inc, 1}),
            folsom_metrics:notify(
                {osc_net, recvs, Transport}, {inc, 1}
            ),
            {Data, Buffer} = Parse(Data0, PrevBuffer),
            spawn(fun() -> update(OwnerID, Data) end),
            loop(Socket, Transport, Parse, State#state{buffer=Buffer});
        Error ->
            lager:error(
                "Got error ~p when attempting to receive from socket ~p",
                [Error, Socket]
            ),
            ok = Transport:close(Socket)
    end.

update(OwnerID, Data) ->
    Grouped = dict:to_list(lists:foldl(
        fun({Metric, Timestamp, Value}, Acc) ->
            %% TODO: should this dereference the Metric to an ID?
            Key = {OwnerID, Metric},
            case dict:find(Key, Acc) of
                {ok, Points} ->
                    dict:store(Key, [{Timestamp, Value}|Points], Acc);
                error ->
                    dict:store(Key, [{Timestamp, Value}], Acc)
             end
         end,
         dict:new(),
         Data
     )),
     update_int(Grouped).

update_int([]) ->
    ok;
update_int([{Metric, Points}|Xs]) ->
    case osc:update(Metric, Points) of
        ok ->
            ok;
        Else ->
            lager:error(
                "Got ~p when attempting to update for metric ~p",
                [Else, Metric]
            )
    end,
    update_int(Xs).
