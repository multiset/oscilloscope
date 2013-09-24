-module(oscilloscope_cache).

-export([
    start/0,
    stop/0,
    process/3,
    read/3
]).

-export([
    start_link/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(st, {
    metric,
    resolution,
    aggregation_fun,
    last_persist
}).

%% Minimim amount of seconds to wait before persisting to disk
-define(MIN_PERSIST_AGE, 300).

%% TODO: compress
-define(VALENCODE(V), term_to_binary(V)).
-define(VALDECODE(V), binary_to_term(V)).

%% Number of bytes to store in each value
-define(MIN_CHUNK_SIZE, 1000).
-define(MAX_CHUNK_SIZE, 1024).

start() ->
    application:start(oscilloscope_cache).

stop() ->
    ok.

process(Metric, Timestamp, Value) ->
    multicast(Metric, {process, Timestamp, Value}).

read(Metric, From, Until) ->
    {Megaseconds, Seconds, _} = erlang:now(),
    QueryTime = Megaseconds * 1000000 + Seconds,
    Resolutions = multicall(Metric, get_resolution),
    Resolutions1 = lists:sort(
        fun({_, {ok, {IntervalA, _}}}, {_, {ok, {IntervalB, _}}}) ->
            IntervalA >= IntervalB
        end,
        Resolutions
    ),
    {Pid, _Resolution} = lists:foldl(
        fun({_, {ok, {Interval, Count}}}=R, Result) ->
            case QueryTime - Interval * Count < From of
                true -> R;
                false -> Result
            end
        end, hd(Resolutions1), Resolutions1),
    gen_server:call(Pid, {read, From, Until}).

start_link(Metric, Resolution) ->
    gen_server:start_link(?MODULE, {Metric, Resolution}, []).

init({Metric, Resolution}) ->
    ok = pg2:join(Metric, self()),
    %% TODO: get these from persistent store
    AggregationFun = fun oscilloscope_cache_aggregations:avg/1,
    LastPersist = 0,
    case read({Metric, Resolution}) of
        not_found ->
            write({Metric, Resolution}, {undefined, array:new({default, null})});
        _ ->
            ok
    end,
    {ok, #st{
        metric = Metric,
        resolution = Resolution,
        aggregation_fun = AggregationFun,
        last_persist = LastPersist
    }}.

handle_call(get_resolution, _From, #st{resolution=R}=State) ->
    {reply, {ok, R}, State};
handle_call({read, From, Until}, _From, State) ->
    #st{metric=M, aggregation_fun=AF, resolution={I, _C}=R} = State,
    {T0, Points} = read({M, R}),
    %% FIXME: this method leads us to read one index too far back
    %% unless `From rem I == 0`.
    StartIndex = (From - T0) div I,
    EndIndex = (Until - T0) div I,
    Acc0 = case StartIndex < 0 of
        true -> lists:duplicate(abs(StartIndex), null);
        false -> []
    end,
    InRange = array:foldl(
        fun(Index, Vs, Acc) ->
            case Index >= StartIndex andalso Index =< EndIndex of
                true -> [AF(Vs)|Acc];
                false -> Acc
            end
        end, Acc0, Points
    ),
    MissingTail = (EndIndex - StartIndex) - (length(InRange) - 1),
    Full = case MissingTail > 0 of
        true ->
            lists:duplicate(MissingTail, null) ++ InRange;
        false ->
            InRange
    end,
    RealFrom = T0 + StartIndex * I,
    RealUntil = T0 + EndIndex * I,
    Reply = [
        {from, RealFrom},
        {until, RealUntil},
        {interval, I},
        {datapoints, lists:reverse(Full)}
    ],
    {reply, {ok, Reply}, State};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, error, State}.

handle_cast({process, Timestamp, Value}, State) ->
    #st{metric=M, resolution={I, _C}=R, last_persist=LP} = State,
    Timestamp1 = Timestamp - (Timestamp rem I),
    %% We claim a ?MIN_PERSIST_AGE maximum age
    %% but that's enforced by not persisting newer points.
    %% In practice we'll accept anything that's newer than the last persist.
    %% ^^ I have no idea what this comment means, but I suspect it's sensible
    if
        Timestamp1 > LP ->
            {T0, OldPoints} = read({M, R}),
            {T1, Index} = case T0 of
                undefined ->
                    {Timestamp1, 0};
                _ ->
                    {T0, (Timestamp1 - T0) div I}
            end,
            NewPoints = case array:get(Index, OldPoints) of
                null ->
                    array:set(Index, [Value], OldPoints);
                Vs ->
                    array:set(Index, [Value|Vs], OldPoints)
            end,
            write({M, R}, {T1, NewPoints});
        true ->
            ok
    end,
    {noreply, State, 0};
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(timeout, State) ->
    {noreply, maybe_persist_points(State)};
handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% TODO: this is a disaster
maybe_persist_points(#st{}=State) ->
    #st{metric=M, resolution={I, _C}=R, aggregation_fun=AF} = State,
    {T0, Points} = read({M, R}),
    {Megaseconds, Seconds, _} = erlang:now(),
    Now = Megaseconds * 1000000 + Seconds,
    %% We only persist up to the epoch now - ?MIN_PERSIST_AGE
    LatestPersistTime = Now - ?MIN_PERSIST_AGE,
    PersistIndex = (LatestPersistTime - (LatestPersistTime rem I) - T0) / I,
    %% Split the array - points old enough to persist on the left, etc
    {ToPersist, ToMem} = array:foldr(
        fun(Idx, Value, {Persist, Mem}) ->
            case Idx =< PersistIndex of
                true -> {[Value|Persist], Mem};
                false -> {Persist, [Value|Mem]}
            end
        end,
        [],
        Points
    ),
    %% Split the points old enough to persist in to chunks of 1k
    {Remainder, PersistChunks} = chunkify(ToPersist, AF),
    %% Persist the relevant 1k chunks, crash if there's a failure
    %% http://cl.ly/image/1h1y1U0L2L1G <- :rageface:
    OKs = lists:duplicate(length(ToPersist), ok),
    OKs = lists:map(
       fun(Index, Value) -> T = T0 + (Index * I), persist(T, Value) end,
       PersistChunks
    ),
    %% TODO: Persist what timestamps we persisted
    %% Merge extras back with the points to remain in memory
    ToMem1 = Remainder ++ ToMem,
    T1 = LatestPersistTime - (I * length(Remainder)),
    write({M, R}, {T1, array:from_list(ToMem1)}),
    State#st{last_persist=Now}.

persist(_Timestamp, _Value) ->
    %% TODO
    ok.

-spec chunkify([number()], fun()) -> {[number()], [{integer(), binary()}]}.
chunkify(ToChunk, Aggregator) ->
    chunkify(ToChunk, 0, [], [], Aggregator).

chunkify([], _Index, ThisChunk, Chunked, _Aggregator) ->
    {ThisChunk, Chunked};
chunkify([V|Vs], Index, ThisChunk, Chunked, Aggregator) ->
    Encoded = ?VALENCODE(lists:reverse(ThisChunk)),
    {ThisChunk1, Chunked1} = case byte_size(Encoded) of
        Size when Size >= ?MIN_CHUNK_SIZE andalso Size =< ?MAX_CHUNK_SIZE ->
            {[Aggregator(V)], [{Index, Encoded}|Chunked]};
        Size when Size < ?MIN_CHUNK_SIZE ->
            {[Aggregator(V)|ThisChunk], Chunked}
    end,
    chunkify(Vs, Index + 1, ThisChunk1, Chunked1, Aggregator).

multicast(Metric, Msg) ->
    Pids = get_pids(Metric),
    lists:map(fun(P) -> gen_server:cast(P, Msg) end, Pids).

multicall(Metric, Msg) ->
    Pids = get_pids(Metric),
    lists:map(fun(P) -> {P, gen_server:call(P, Msg)} end, Pids).

get_pids(Metric) ->
    case pg2:get_members(Metric) of
        {error, {no_such_group, Metric}} ->
            {ok, _Pid} = oscilloscope_cache_sup:spawn_cache(Metric),
            pg2:get_members(Metric);
        P ->
            P
    end.

read(Key) ->
    case erp:q(["GET", term_to_binary(Key)]) of
        {ok, undefined} -> not_found;
        {ok, Value} -> ?VALDECODE(Value)
    end.

write(Key, Value) ->
    {ok, <<"OK">>} = erp:q(["SET", term_to_binary(Key), ?VALENCODE(Value)]),
    ok.
