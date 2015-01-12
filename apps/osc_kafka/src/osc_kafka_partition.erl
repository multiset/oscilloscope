-module(osc_kafka_partition).

-behaviour(gen_fsm).

-export([name/1]).

-export([start_link/1]).

-export([
    init/1,
    recv/2,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4
]).

-record(state, {
    partition,
    offset=0,
    timeout=1000,
    topic_name
}).


start_link(Partition) ->
    gen_fsm:start_link({local, name(Partition)}, ?MODULE, [Partition], []).


init([Partition]) ->
    {ok, Topic} = application:get_env(osc_kafka, topic_name),
    {ok, recv, #state{partition=Partition, topic_name=Topic}, 0}.


recv(timeout, State) ->
    #state{
        partition=Partition,
        offset=OldOffset,
        timeout=Timeout,
        topic_name=Topic
    } = State,
    case kofta:fetch(Topic, Partition, [{offset, OldOffset}]) of
        {ok, []} ->
            {next_state, recv, State, Timeout};
        {ok, Messages} ->
            {Batch, NewOffset} = lists:foldl(fun({Offset, Key, BValue}, Acc) ->
                {BatchAcc, _Offs} = Acc,
                <<Time:32/integer, Rest/binary>> = Key,
                <<Value:64/float>> = BValue,
                Metric = case Rest of
                    <<1:8/integer, MetricID:32/integer>> ->
                        MetricID;
                    <<0:8/integer, OwnerID:32/integer, NameBin/binary>> ->
                        {OwnerID, NameBin}
                end,
                {[{Metric, Time, Value}|BatchAcc], Offset}
            end, {[], 0}, Messages),
            ok = osc_kafka_insert_sup:start_child(Batch),
            {next_state, recv, State#state{offset=NewOffset+1}, Timeout};
        {error, Reason} ->
            lager:error("Error fetching partition ~p: ~p", [Partition, Reason]),
            {next_state, recv, State, Timeout}
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


name(PartitionID) ->
    list_to_atom("osc_kafka_partition_" ++ integer_to_list(PartitionID)).
