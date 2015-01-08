-module(osc_kafka_partition).

-behaviour(gen_fsm).

-export([start_link/1]).

-export([
    init/1,
    recv/2,
    retry_batch/2,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4
]).

-record(state, {
    partition,
    offset=0,
    timeout,
    retry_batch
}).


start_link(Partition) ->
    Name = list_to_atom("osc_kafka_partition_" ++ integer_to_list(Partition)),
    gen_fsm:start_link({local, Name}, ?MODULE, [Partition], []).


init([Partition]) ->
    {ok, recv, #state{partition=Partition}, 0}.


recv(timeout, State) ->
    #state{
        partition=Partition,
        offset=OldOffset,
        timeout=Timeout
    } = State,
    {ok, Topic} = application:get_env(osc_kafka, topic_name),
    case kofta:fetch(Topic, Partition, [{offset, OldOffset}]) of
        {ok, []} ->
            {next_state, recv, State, Timeout};
        {ok, Messages} ->
            {Batch, NewOffset} = lists:foldl(fun({Offset, Key, BValue}, Acc) ->
                {BatchAcc, _Offs} = Acc,
                <<Time:32/integer, OwnerID:32/integer, Rest/binary>> = Key,
                <<Value:64/float>> = BValue,
                Message = case Rest of
                    <<1:8/integer, MetricID:32/integer>> ->
                        {MetricID, Time, Value};
                    <<0:8/integer, NameBin/binary>> ->
                        {OwnerID, binary_to_term(NameBin), Time, Value}
                end,
                {[Message|BatchAcc], Offset}
            end, {[], 0}, Messages),
            NewState = State#state{offset=NewOffset+1},
            {ok, BatchTimeout} = application:get_env(osc_kafka, batch_timeout),
            case osc_kafka_router:send(Batch, BatchTimeout) of
                ok ->
                    {next_state, recv, NewState, Timeout};
                {error, Reason} ->
                    lager:error("Error sending batch to router: ~p", [Reason]),
                    {
                        next_state,
                        retry_batch,
                        NewState#state{retry_batch=Batch},
                        Timeout
                    }
            end;
        {error, Reason} ->
            lager:error("Error fetching partition ~p: ~p", [Partition, Reason]),
            {next_state, recv, State, Timeout}
    end.


retry_batch(timeout, State) ->
    #state{
        retry_batch=Batch,
        timeout=Timeout
    } = State,
    case osc_kafka_router:send(Batch) of
        ok ->
            {next_state, recv, State#state{retry_batch=undefined}, Timeout};
        {error, Reason} ->
            lager:error("Error sending batch to router: ~p", [Reason]),
            {next_state, retry_batch, State, Timeout}
    end.


handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.


handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.


handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.


terminate(_Reason, _StateName, _State) ->
    ok.


code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
