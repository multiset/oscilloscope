-module(osc_kafka_partition).

-behaviour(gen_fsm).

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
    timeout=0
}).


start_link(Partition) ->
    Name = list_to_atom("osc_kafka_partition_" ++ integer_to_list(Partition)),
    gen_fsm:start_link({local, Name}, ?MODULE, [Partition], []).


init([Partition]) ->
    {ok, recv, #state{partition=Partition}, 0}.


recv(timeout, State) ->
    #state{
        partition=Partition,
        offset=OldOffset
    } = State,
    NewState = case kofta:fetch(<<"porter">>, Partition, [{offset, OldOffset}]) of
        {ok, []} ->
            State#state{timeout=5000};
        {ok, Messages} ->
            NewOffset = lists:foldl(fun({Offset, Key, Value}, _Offset) ->
                <<Time:32/integer, OwnerID:32/integer, Suffix/binary>> = Key,
                MetricID = case Suffix of
                    <<1:8/integer, MetricID0:32/integer>> ->
                        MetricID0;
                    <<0:8/integer, NameBin/binary>> ->
                        Name = binary_to_term(NameBin),
                        case osc_meta_metric:create({OwnerID, Name}) of
                            {ok, MetricID0} ->
                                MetricID0;
                            {error, {exists, MetricID0}} ->
                                MetricID0
                        end
                end,
                <<FloatValue:64/float>> = Value,
                ok = osc:update(MetricID, [{Time, FloatValue}]),
                Offset
            end, 0, Messages),
            State#state{offset=NewOffset+1, timeout=0};
        {error, _Reason} ->
            % TODO: Log
            State#state{timeout=0}
    end,
    {next_state, recv, NewState, NewState#state.timeout}.


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
