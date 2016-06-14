-module(osc_persistence_manager).
-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    submit_events/1
]).

-record(st, {
    last_control,
    error,
    integral,
    timer,
    kd,
    ki,
    kp,
    statistics,
    concurrency,
    min_persist_size,
    queue
}).


-spec submit(Events) -> ok when
    Events :: [tuple()].

submit_events(Events) ->
    gen_server:call(?MODULE, {submit, Events}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    {ok, Dt} = application:get_env(osc_persistence, control_frequency),
    {ok, Kd} = application:get_env(osc_persistence, control_kd),
    {ok, Ki} = application:get_env(osc_persistence, control_ki),
    {ok, Kp} = application:get_env(osc_persistence, control_kp),
    {ok, MinSize} = application:get_env(osc_persistence, min_persist_size),
    {ok, TRef} = timer:send_interval(Dt, control),
    ok = register_listener(),
    State = #st{
        last_control = os:timestamp(),
        error = 0,
        integral = 0,
        timer = TRef,
        kd = Kd,
        ki = Ki,
        kp = Kp,
        statistics = ets:new(?MODULE, [public]),
        concurrency = 0,
        min_persist_size = MinSize,
        queue = pqueue4:new()
    },
    {ok, State}.


handle_call({submit, Events}, From, State) ->
    lists:foldl(
        fun(E, {_, _, S0}) -> handle_call(E, From, S0) end,
        {reply, ok, State},
        Events
    );
handle_call({window_update, MM, WM, Data, _}, _From, State) ->
    #st{statistics = Tab, queue = Queue0} = State,
    {Key, Window} = update(point_count, apod:size(Data), MM, WM, Tab),
    Queue1 = prioritize(Key, Window, Queue0),
    {reply, ok, State#st{queue=Queue1}};
handle_call({cache_read, MM, WM, _}, _From, State) ->
    #st{statistics = Tab, queue = Queue0} = State,
    {Key, Window} = update(last_read, os:timestamp(), MM, WM, Tab),
    Queue1 = prioritize(Key, Window, Queue0),
    {reply, ok, State#st{queue=Queue1}};
handle_call(Msg, _From, State) ->
    lager:warning(
        "osc_persistence_event_manager received unknown call: ~p",
        [Msg]
    ),
    {noreply, State}.


handle_cast(Msg, State) ->
    lager:warning(
        "osc_persistence_event_manager received unknown cast: ~p",
        [Msg]
    ),
    {noreply, State}.


handle_info({gen_event_EXIT, _, normal}, State) ->
    {stop, normal, State};
handle_info({gen_event_EXIT, _, shutdown}, State) ->
    {stop, shutdown, State};
handle_info({gen_event_EXIT, _, Reason}, State) ->
    lager:warning(
        "osc_persistence_event_listener died with reason ~p; restarting it.",
        [Reason]
    ),
    ok = register_listener(),
    {noreply, State};
handle_info(control, State0) ->
    #st{
        last_control = T0,
        last_retry_count = Retries0,
        error = Error0,
        integral = Integral0,
        kd = Kd,
        ki = Ki,
        kp = Kp,
        concurrency = WorkerTarget0,
        min_persist_size = MinPersistSize0,
        queue = Queue0
    } = State0,
    T1 = os:timestamp(),
    Dt = timer:now_diff(T1, T0) / 1000,
    {ok, Target} = application:get_env(osc_persistence, control_memory_target),
    Value = erlang:memory(total),
    Error1 = Value - Target,
    Integral1 = Integral0 + Error1 * Dt,
    Derivative = (Error1 - Error0) / Dt,
    Delta = trunc(Kp * Error1 + Ki * Integral1 + Kd * Derivative),
    %% If Delta > 0, we need to increase throughput; otherwise we need to reduce
    %% it. There are two knobs available to do this: concurrency and minimum
    %% size.
    %%
    %% Concurrency is the "default" control, because it's effectively free -
    %% we're already paying for the R/W capacity we have, so we can increase
    %% concurrency to take advantage of it. It only works up to a point, though
    %% - note that this implementation does *not* automatically scale R/W
    %% capacity within DynamoDB. Therefore the effectiveness of concurrency
    %% increases is limited. We can identify that limit by observing the
    %% frequency of rate limited requests.
    %%
    %% Once the concurrency limit is reached, we must reduce the minimum persist
    %% size. This allows less-than-full chunks to be sent to DynamoDB. This is
    %% costly - we'll incur elevated price-per-datapoint on both the write *and*
    %% all subsequent reads - but it's preferable to running the machine out of
    %% memory.
    %%
    %% These constraints lead us to the following implementation:
    %%
    %% A. If more throughput is required, increase concurrency if it appears
    %% that will be effective. If more concurrency is not available, reduce
    %% persist size.
    %%
    %% B. If less throughput is required, start by increasing persist size up to
    %% the initial value (from the application environment). If the persist size
    %% is unmodified, then reduce concurrency.
    case Delta > 0 of
        true ->
            

    WorkerTarget1 = max(0, WorkerTarget0 + Delta),
    {WorkerTarget2, MinPersistSize1} = case WorkerTarget1 > MaxConcurrency of
        true ->
            {MaxConcurrency, 0};
        false ->
            {WorkerTarget1, MinPersistSize0}
    end,
    Queue1 = maybe_spawn_workers(WorkerTarget2, MinPersistSize1, Queue0),
    State1 = State0#st{
        last_control = T1,
        error = Error1,
        integral = Integral1,
        concurrency = WorkerTarget1,
        queue = Queue1
    },
    {noreply, State1};
handle_info({'DOWN', Ref, process, Pid, Info}, State) ->
    #st{concurrency=Target, min_persist_size=Size, queue=Queue0} = State,
    Queue1 = maybe_spawn_workers(Target, Size, Queue0),
    {noreply, State#st{queue = Queue1}};
handle_info(Msg, State) ->
    lager:warning(
        "osc_persistence_event_manager received unknown info: ~p",
        [Msg]
    ),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


register_listener() ->
    osc_event:register(osc_persistence_event_listener).


update(Prop, Value, MetricMeta, WindowMeta, Tab) ->
    Key = {osc_meta_metric:id(MetricMeta), osc_meta_window:id(WindowMeta)},
    Window0 = case ets:lookup(Tab, Key) of
        [] ->
            maps:new();
        [{Key, W}] ->
            W
    end,
    Window1 = maps:update(Prop, Value, Window0),
    true = ets:insert(Tab, {Key, Window1}),
    {Key, Window1}.


prioritize(Key, Window, Queue0) ->
    {ok, ReadWeight} = application:get_env(osc_persistence, read_weight),
    {ok, SizeWeight} = application:get_env(osc_persistence, size_weight),
    {ok, Penalty} = application:get_env(osc_persistence, chunk_size_penalty),
    {ok, MinSize} = application:get_env(osc_persistence, min_chunk_size),
    %% Calculate weight based on read frequency
    LastRead = maps:get(last_read, Window, {0, 0, 0}),
    Read0 = timer:now_diff(os:timestamp(), LastRead) / 1000 / 1000,
    % Bound the "no reads" range to an hour, and scale it down
    Read1 = trunc((min(Read0, 3600) / 3600) * (ReadWeight * 128)),
    %% Calculate weight based on size
    Count = maps:get(point_count, Window, 0),
    Size = trunc((min(Count, 100000) / 100000) * (SizeWeight * 128)),
    %% Penalize the priority if we don't think there are enough points to fill a
    %% chunk of the appropriate size. We don't want to *never* persist these
    %% too-small chunks, just do them very last (and sorted by in-memory size).
    BytesPerPoint = maps:get(bytes_per_point, Window, 8),
    Priority = case (Count * BytesPerPoint) > MinSize of
        true ->
            Read1 + Size;
        false ->
            Read1 + Size - Penalty
    end,
    pqueue4:in(Key, Priority, Queue0).


maybe_spawn_workers(Target, MinSize, Queue0) ->
    Children = supervisor:which_children(osc_persistence_worker_sup),
    case length(Children) >= Target of
        true ->
            Queue0;
        false ->
            case pqueue4:out(Queue0) of
                {empty, Queue0} ->
                    Queue0;
                {{value, {MetricID, WindowID}}, Queue1} ->
                    {ok, Pid} = osc_persistence_worker_sup:start_child(
                        MetricID, WindowID, MinSize
                    ),
                    erlang:monitor(process, Pid),
                    maybe_spawn_workers(Target, Queue1)
            end
    end.
