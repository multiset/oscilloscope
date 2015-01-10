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

    dict:map(fun(Metric, Datapoints) ->
        {ok, Pid} = case osc_cache:find(Metric) of
            {ok, Pid0} ->
                {ok, Pid0};
            not_found ->
                case osc_cache:start(Metric) of
                    {ok, Pid1} ->
                        {ok, Pid1};
                    not_found ->
                        osc_kafka_metric_creator_sup:start_child(Metric)
                end
        end,
        ok = osc_cache:update(Pid, Datapoints)
    end, GroupedMessages),
    State#state{batch=[]}.
