-module(osc_kafka_router).

-behaviour(gen_server).

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
    batch
}).


start_link(Batch) ->
    gen_server:start_link(?MODULE, [Batch], []).


init([Batch]) ->
    {ok, #state{batch=Batch}, 0}.


handle_call(Msg, _From, State) ->
    lager:error("Received unknown message: ~p", [Msg]),
    {stop, unknown_message, State}.


handle_cast(Msg, State) ->
    lager:error("Received unknown message: ~p", [Msg]),
    {stop, unknown_message, State}.


handle_info(timeout, State) ->
    case send_batch(State) of
        #state{batch=[]}=NewState ->
            {stop, normal, NewState};
        NewState ->
            {noreply, NewState, 1000}
    end.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec send_batch(State) -> State when
    State :: #state{}.

send_batch(State) ->
    #state{batch=Batch} = State,
    GroupedMessages = lists:foldl(fun(Message, Acc) ->
        case Message of
            {OwnerID, Name, Timestamp, Value} ->
                dict:append({OwnerID, Name}, {Timestamp, Value}, Acc);
            {MetricID, Timestamp, Value} ->
                dict:append(MetricID, {Timestamp, Value}, Acc)
        end
    end, dict:new(), Batch),

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
                        % If the metric creator proc terminates between the
                        % calls to find and update, the update will noproc.
                        % Retry later instead.
                        lists:foldl(fun({Timestamp, Value}, Acc) ->
                            [{OwnerID, Name, Timestamp, Value}|Acc]
                        end, RetryAcc, Datapoints)
                end;
            MetricID ->
                ok = osc_cache:update(MetricID, Datapoints),
                RetryAcc
        end
    end, [], GroupedMessages),
    #state{batch=Retry}.
