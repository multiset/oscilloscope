-module(osc_kafka_router).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([send/1]).

-export([start_link/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    messages,
    max_time,
    last_batch
}).


-spec send([Message]) -> ok | {error, Reason} when
    Message :: {MetricID, Time, Value} | {OwnerID, Name, Time, Value},
    MetricID :: integer(),
    Time :: integer(),
    Value :: float(),
    OwnerID :: integer(),
    Name :: [{binary(), binary()}],
    Reason :: any().

send(Messages) ->
    poolboy:transaction(?MODULE, fun(Worker) ->
        gen_server:call(Worker, {batch, Messages})
    end).


start_link([]) ->
    gen_server:start_link(?MODULE, [], []).


init([]) ->
    {ok, #state{last_batch=os:timestamp(), max_time=60000, messages=[]}}.


handle_call({batch, Batch}, _From, State) ->
    #state{
        messages=Messages
    } = State,
    {NewState, Timeout} = maybe_send_batch(State#state{messages=[Batch|Messages]}),
    {reply, ok, NewState, Timeout}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(timeout, State) ->
    {NewState, Timeout} = maybe_send_batch(State),
    {noreply, NewState, Timeout}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec maybe_send_batch(State) -> {State, Timeout} when
    State :: #state{},
    Timeout :: integer().

maybe_send_batch(State) ->
    #state{
        last_batch=LastBatch,
        max_time=MaxTime
    } = State,
    case timer:now_diff(os:timestamp(), LastBatch) of
        Diff when Diff >= MaxTime ->
            {send_batch(State), MaxTime};
        Diff ->
            {State, MaxTime-Diff}
    end.


-spec send_batch(State) -> State when
    State :: #state{}.

send_batch(State) ->
    #state{messages=Messages} = State,
    FlatMessages = lists:flatten(Messages),

    GroupedMessages = lists:foldl(fun(Message, Acc) ->
        case Message of
            {OwnerID, Name, Timestamp, Value} ->
                dict:append({OwnerID, Name}, {Timestamp, Value}, Acc);
            {MetricID, Timestamp, Value} ->
                dict:append(MetricID, {Timestamp, Value}, Acc)
        end
    end, dict:new(), FlatMessages),

    Retry = dict:fold(fun(Key, Datapoints, RetryAcc) ->
        case Key of
            {OwnerID, Name} ->
                Pid = case osc_kafka_metric_creator:find(OwnerID, Name) of
                    undefined ->
                        % TODO: handle not {ok, Pid}
                        {ok, Pid0} = osc_kafka_metric_creator_sup:start_child(
                            OwnerID,
                            Name
                        ),
                        Pid0;
                    Pid0 ->
                        Pid0
                end,
                case osc_kafka_metric_creator:update(Pid, Datapoints) of
                    ok ->
                        RetryAcc;
                    {error, noproc} ->
                        lists:foldl(fun({Timestamp, Value}, Acc) ->
                            [{OwnerID, Name, Timestamp, Value}|Acc]
                        end, RetryAcc, Datapoints)
                end;
            MetricID ->
                ok = osc_cache:update(MetricID, Datapoints),
                RetryAcc
        end
    end, [], GroupedMessages),

    State#state{messages=Retry, last_batch=os:timestamp()}.
