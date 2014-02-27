-module(oscilloscope_cache).

-export([
    start/0,
    stop/0,
    process/5,
    read/5
]).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").
-include("oscilloscope_cache.hrl").

-record(st, {
    resolution_id :: resolution_id(),
    group :: group(),
    interval :: interval(),
    count :: count(),
    persisted :: persisted(),
    aggregation_fun :: fun(),
    commutator,
    min_chunk_size :: pos_integer(),
    max_chunk_size :: pos_integer(),
    min_persist_age :: pos_integer(),
    last_touch :: {non_neg_integer(),non_neg_integer(),non_neg_integer()}
}).

start() ->
    application:start(oscilloscope_cache).

stop() ->
    ok.

-spec process(userid(), service(), host(), timestamp(), float()) -> any().
process(UserID, Name, Host, Timestamp, Value) ->
    lager:debug(
        "Processing point: ~p ~p ~p ~p ~p",
        [UserID, Name, Host, Timestamp, Value]
    ),
    multicast(UserID, Name, Host, {process, Timestamp, Value}).

-spec read(userid(), service(), host(), timestamp(), timestamp()) ->
  {ok, [{atom, any()}]}.
read(UserID, Name, Host, From, Until) ->
    lager:debug(
        "Processing read: ~p ~p ~p ~p ~p",
        [UserID, Name, Host, From, Until]
    ),
    CacheMetadata = multicall(UserID, Name, Host, get_metadata),
    Pid = select_pid_for_query(CacheMetadata, From),
    gen_server:call(Pid, {read, From, Until}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    {
        Group,
        ResolutionId,
        Interval,
        Count,
        Persisted,
        AggregationAtom,
        Commutator,
        MinChunkSize,
        MaxChunkSize,
        MinPersistAge
    } = Args,
    lager:info(
        "Booting cache pid ~p for group ~p, interval ~p, count ~p",
        [self(), Group, Interval, Count]
    ),
    folsom_metrics:notify({oscilloscope_cache, cache_inits}, {inc, 1}),
    AggregationFun = fun(Vals) ->
        erlang:apply(oscilloscope_cache_aggregations, AggregationAtom, [Vals])
    end,
    {ok, C} = commutator:connect(
        proplists:get_value(table, Commutator),
        proplists:get_value(schema, Commutator),
        proplists:get_value(region, Commutator),
        proplists:get_value(accesskey, Commutator),
        proplists:get_value(secretkey, Commutator)
    ),
    State = #st{
        resolution_id = ResolutionId,
        group = Group,
        interval = Interval,
        count = Count,
        persisted = Persisted,
        aggregation_fun = AggregationFun,
        commutator = C,
        min_chunk_size = MinChunkSize,
        max_chunk_size = MaxChunkSize,
        min_persist_age = MinPersistAge,
        last_touch = erlang:now()
    },
    case oscilloscope_cache_memory:read(State#st.resolution_id) of
        not_found ->
            %% TODO: get T0 (undefined here) from persistent store
            folsom_metrics:notify({oscilloscope_cache, mem_inits}, {inc, 1}),
            oscilloscope_cache_memory:write(
                State#st.resolution_id,
                {undefined, array:new({default, null})}
            );
        _ ->
            ok
    end,
    {ok, State}.

handle_call(get_metadata, _From, State) ->
    #st{
         interval=Interval,
         count=Count,
         last_touch=LastTouch
    } = State,
    {T0, Points} = oscilloscope_cache_memory:read(State#st.resolution_id),
    {EarliestTime, LatestTime} = case T0 of
        undefined ->
            {undefined, undefined};
        _ ->
            L = timestamp_from_index(T0, array:size(Points) - 1, Interval),
            E = L - Interval * Count,
            {E, L}
    end,
    Metadata = [
        {earliest_time, EarliestTime},
        {latest_time, LatestTime},
        {interval, Interval},
        {count, Count},
        {last_touch, LastTouch},
        {aggregation_fun, State#st.aggregation_fun}
    ],
    {reply, {ok, Metadata}, State};
handle_call({read, From0, Until0}, _From, State) ->
    #st{
        resolution_id=Id,
        interval=Interval,
        persisted=Persisted,
        aggregation_fun=AF,
        commutator=Commutator
    } = State,
    folsom_metrics:notify({oscilloscope_cache, reads}, {inc, 1}),
    {T0, Points} = oscilloscope_cache_memory:read(State#st.resolution_id),
    {From, Until} = calculate_query_bounds(From0, Until0, Interval),
    Cached = cached_read(From, Until, Interval, AF, T0, Points),
    Data = case T0 > From of
        false ->
            Cached;
        true ->
            folsom_metrics:notify(
                {oscilloscope_cache, persistent_reads},
                {inc, 1}
            ),
            Disk = persistent_read(
                Id,
                Commutator,
                From,
                Until,
                Interval,
                Persisted
            ),
            DiskCount = length(Disk),
            CachedNonNull = length(Cached) - DiskCount,
            Disk ++ lists:sublist(Cached, DiskCount + 1, CachedNonNull)
    end,
    folsom_metrics:notify(
        {oscilloscope_cache, points_read},
        {inc, length(Data)}
    ),
    Reply = [
        {from, From},
        {until, Until},
        {interval, Interval},
        {datapoints, Data}
    ],
    {reply, {ok, Reply}, State#st{last_touch = erlang:now()}};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, error, State}.

handle_cast({process, Timestamp, Value}, State) ->
    #st{
        resolution_id=ResolutionId,
        interval=Interval,
        count=Count,
        persisted=Persisted,
        min_persist_age=MinPersistAge
    } = State,
    {T0, Points0} = oscilloscope_cache_memory:read(ResolutionId),
    {T1, Points1} = process(
        Timestamp,
        Value,
        T0,
        Points0,
        Interval,
        Persisted,
        MinPersistAge
    ),
    {T2, Points2} = maybe_trim_points(T1, Points1, Interval, Count),
    oscilloscope_cache_memory:write(ResolutionId, {T2, Points2}),
    folsom_metrics:notify({oscilloscope_cache, points_processed}, {inc, 1}),
    {noreply, State, 0};
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(timeout, State) ->
    #st{
        resolution_id=Id,
        interval=Interval,
        persisted=Persisted,
        aggregation_fun=AggFun,
        commutator=Commutator,
        min_chunk_size=MinChunkSize,
        max_chunk_size=MaxChunkSize,
        min_persist_age=MinPersistAge
    } = State,
    {T0, Points} = oscilloscope_cache_memory:read(State#st.resolution_id),
    {ToPersist, T1, ToCache} = split_for_persisting(
        T0,
        Points,
        Interval,
        AggFun,
        MinChunkSize,
        MaxChunkSize,
        MinPersistAge
    ),
    TimestampsPersisted = case length(ToPersist) of
        0 ->
            folsom_metrics:notify(
                {oscilloscope_cache, null_persists},
                {inc, 1}
            ),
            [];
        N ->
            folsom_metrics:notify({oscilloscope_cache, persists}, {inc, 1}),
            folsom_metrics:notify(
                {oscilloscope_cache, points_persisted},
                {inc, N}
            ),
            persist(ToPersist, Id, Commutator)
    end,
    oscilloscope_cache_memory:write(State#st.resolution_id, {T1, ToCache}),
    State1 = State#st{
        persisted = Persisted ++ TimestampsPersisted,
        last_touch = erlang:now()
    },
    {noreply, State1};
handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

select_pid_for_query(Metadata, From) ->
    [{Pid0, _}|Ms] = lists:sort(
        fun({_, {ok, MetaA}}, {_, {ok, MetaB}}) ->
            IntervalA = proplists:get_value(interval, MetaA),
            IntervalB = proplists:get_value(interval, MetaB),
            IntervalA >= IntervalB
        end,
        Metadata
    ),
    lists:foldl(
        fun({P, {ok, Meta}}, Acc) ->
            case proplists:get_value(earliest_time, Meta) =< From of
                true -> P;
                false -> Acc
            end
    end, Pid0, Ms).

process(Timestamp, Value, T0, Points, Interval, Persisted, MinPersistAge) ->
    %% Timestamps are always floored to fit intervals exactly
    Timestamp1 = Timestamp - (Timestamp rem Interval),
    %% We claim a MinPersistAge maximum age
    %% but that's enforced by not persisting newer points.
    %% In practice we'll accept anything that's newer than the last persist.
    LastPersist = case Persisted of
        [] ->
            0;
        Persisted ->
            {LastPersistTime, _LastPersistCount} = lists:last(Persisted),
            LastPersistTime
    end,
    case Timestamp1 > LastPersist of
        true ->
            case T0 of
                undefined ->
                    {Timestamp1, append_point(0, Value, Points)};
                T when T =< Timestamp1 ->
                    Index = (Timestamp1 - T0) div Interval,
                    {T0, append_point(Index, Value, Points)};
                T when T - MinPersistAge =< Timestamp1 ->
                    %% Need to slide the window backwards - this is a legal
                    %% point that's at a negative index in the current array.
                    %% N.B.: this is exploitable for metrics where no points
                    %% have been persisted.
                    IndexesToAdd = (T0 - Timestamp1) div Interval,
                    {Timestamp1, prepend_point(IndexesToAdd, Value, Points)};
                _T ->
                    %% Illegal (too old) point, ignore
                    {T0, Points}
            end;
        false ->
            {T0, Points}
    end.

maybe_trim_points(T, Points0, Interval, Count) ->
    LatestTime = T + (array:size(Points0) - 1) * Interval,
    EarliestTime = LatestTime - Interval * (Count - 1), % Inclusive
    SplitIndex = (EarliestTime - T) / Interval,
    case SplitIndex > 0 of
        false ->
            {T, Points0};
        true ->
            {_, Points1} = divide_array(Points0, SplitIndex),
            {EarliestTime, array:from_list(Points1, null)}
    end.

calculate_query_bounds(From0, Until0, Interval) ->
    %% Floor the query's From to the preceding interval bound
    From = From0 - (From0 rem Interval),
    %% Ceil the query's Until to the next interval bound
    Until = case Until0 rem Interval of
        0 -> Until0;
        N -> Until0 + Interval - N
    end,
    {From, Until}.

cached_read(From, Until, Interval, AF, T, Points) when T =< Until ->
    %% At least some of the query is in the cache
    Acc0 = case (T - From) div Interval of
        Count when Count > 0 -> lists:duplicate(Count, null);
        _ -> []
    end,
    StartIndex = (From - T) div Interval,
    EndIndex = (Until - T) div Interval,
    Result = range_from_array(StartIndex, EndIndex, AF, Acc0, Points),
    %% But the cache might not have all the points we need
    Missing = erlang:trunc((((Until - From) / Interval) + 1) - length(Result)),
    Result ++ lists:duplicate(Missing, null);
cached_read(From, Until, Interval, _AF, _T, _Points) ->
    %% There's either no data in the cache, or the full dataset is in the
    %% persistent store
    PointCount = erlang:trunc((Until - From) / Interval),
    lists:duplicate(PointCount, null).

range_from_array(Start, End, AF, Acc0, Points) ->
    lists:reverse(array:foldl(
        fun(I, Vs, Acc) when I >= Start andalso I =< End ->
               [AF(Vs)|Acc];
           (_I, _Vs, Acc) ->
               Acc
        end, Acc0, Points
    )).

prepend_point(Index, Value, Points) ->
    ListPoints = array:to_list(Points),
    Prepend = lists:duplicate(Index, null),
    Points1 = array:from_list(Prepend ++ ListPoints, null),
    append_point(0, Value, Points1).

append_point(Index, Value, Points) ->
    case array:get(Index, Points) of
        null ->
            array:set(Index, [Value], Points);
        Vs when is_list(Vs) ->
            array:set(Index, [Value|Vs], Points)
    end.

split_for_persisting(T0, Points, Interval, AF, MinChunk, MaxChunk, MinPersistAge) ->
    %% Only persist up to the newest point - MinPersistAge
    PersistIndex = erlang:trunc(array:size(Points) - MinPersistAge/Interval),
    case divide_array(Points, PersistIndex) of
        {[], _} ->
            {[], T0, Points};
        {ToPersist, ToMem} ->
            %% Only persist chunks that are large enough
            {Remainder, ChunksToPersist} = chunkify(
                ToPersist,
                AF,
                MinChunk,
                MaxChunk
            ),
            ChunksWithTimestamps = lists:map(
                fun({I, V, S}) ->
                    {timestamp_from_index(T0, I, Interval), V, S}
                end,
                ChunksToPersist
            ),
            %% Merge extras back with the points to remain in memory and
            %% increment the start timestamp for the number of points persisted
            PersistCount = length(ToPersist) - length(Remainder),
            T1 = timestamp_from_index(T0, PersistCount, Interval),
            {ChunksWithTimestamps, T1, array:from_list(Remainder ++ ToMem, null)}
    end.

divide_array(Arr, DivIdx) ->
    array:foldr(
        fun(Idx, Value, {L, R}) ->
            case Idx < DivIdx of
                true -> {[Value|L], R};
                false -> {L, [Value|R]}
            end
        end,
        {[], []},
        Arr
    ).

persist(Points, Id, Commutator) ->
    lists:map(
        fun({Timestamp, Value, Size}) ->
            %% TODO: this will badmatch if, e.g., we're rate-limited
            Start = erlang:now(),
            {ok, true} = commutator:put_item(
                Commutator,
                [Id, Timestamp, Value]
            ),
            Latency = timer:now_diff(erlang:now(), Start),
            folsom_metrics:notify(
                {oscilloscope_cache, persistent_store, write_latency, sliding},
                Latency
            ),
            folsom_metrics:notify(
                {oscilloscope_cache, persistent_store, write_latency, uniform},
                Latency
            ),
            ok = oscilloscope_sql_metrics:insert_persisted(Id, Timestamp, Size),
            {Timestamp, Size}
        end, Points
    ).

persistent_read(_Id, _Commutator, _From, _Until, _Interval, []) ->
    [];
persistent_read(Id, Commutator, From, Until, Interval, Persisted) ->
    StartTime = calculate_starttime(From, Persisted),
    EndTime = calculate_endtime(Until, Persisted),
    Start = erlang:now(),
    {ok, Rows} = commutator:query(
        Commutator,
        [{<<"id">>, equals, [Id]}, {<<"t">>, between, [StartTime, EndTime]}]
    ),
    Latency = timer:now_diff(erlang:now(), Start),
    folsom_metrics:notify(
        {oscilloscope_cache, persistent_store, read_latency, sliding},
        Latency
    ),
    folsom_metrics:notify(
        {oscilloscope_cache, persistent_store, read_latency, uniform},
        Latency
    ),
    Points = lists:flatten(
        [?VALDECODE(proplists:get_value(<<"v">>, I)) || I <- Rows]
    ),
    StartIndex = (From - StartTime) div Interval,
    EndIndex = (Until - EndTime) div Interval,
    lists:sublist(Points, StartIndex, EndIndex - StartIndex + 1).

calculate_starttime(T, Ts) ->
    {Earlier, Later} = lists:partition(fun({T1, _C}) -> T1 =< T end, Ts),
    case Earlier of
        [] ->
            {Time, _Count} = hd(Later),
            Time;
        _Any ->
            {Time, _Count} = lists:last(Earlier),
            Time
    end.

calculate_endtime(T, Ts) ->
    {Later, _Earlier} = lists:partition(fun({T1, _C}) -> T1 >= T end, Ts),
    case Later of
        [] -> T;
        _Any ->
            {Time, _Count} = hd(Later),
            Time
    end.

-spec chunkify([[number()]], fun(), integer(), integer()) ->
  {[[number()]], [{integer(), binary()}]}.
chunkify(Values, Aggregator, ChunkMin, ChunkMax) ->
    {_, Remainder, Chunks} = lists:foldl(
        fun(Value, {Count, Pending, Chunks}) ->
            Pending1 = [Value|Pending],
            %% TODO: lots of lists:reverse here!
            Encoded = ?VALENCODE(lists:reverse(Pending1)),
            case byte_size(Encoded) of
                Size when Size >= ChunkMin andalso Size =< ChunkMax ->
                    PointsChunked = length(Pending1),
                    catch folsom_metrics:notify(
                        {oscilloscope_cache, points_per_chunk, sliding},
                        PointsChunked
                    ),
                    catch folsom_metrics:notify(
                        {oscilloscope_cache, points_per_chunk, uniform},
                        PointsChunked
                    ),
                    catch folsom_metrics:notify(
                        {oscilloscope_cache, bytes_per_chunk, sliding},
                        Size
                    ),
                    catch folsom_metrics:notify(
                        {oscilloscope_cache, bytes_per_chunk, uniform},
                        Size
                    ),
                    {
                        Count + PointsChunked,
                        [],
                        [{Count, Encoded, PointsChunked}|Chunks]
                    };
                Size when Size < ChunkMin ->
                    {Count, Pending1, Chunks}
            end
        end,
        {0, [], []},
        lists:map(Aggregator, Values)
    ),
    RemainingValues = lists:sublist(
        Values, length(Values) - length(Remainder) + 1, length(Values)),
    {RemainingValues, lists:reverse(Chunks)}.

multicast(UserID, Name, Host, Msg) ->
    Pids = get_pids(UserID, Name, Host),
    lists:map(fun(P) -> gen_server:cast(P, Msg) end, Pids).

multicall(UserID, Name, Host, Msg) ->
    Pids = get_pids(UserID, Name, Host),
    lists:map(fun(P) -> {P, gen_server:call(P, Msg)} end, Pids).

get_pids(UserID, Name, Host) ->
    Group = {UserID, Name, Host},
    case oscilloscope_cache_sup:find_group(Group) of
        not_found ->
            oscilloscope_cache_sup:spawn_group(Group);
        Pids ->
            Pids
    end.

timestamp_from_index(InitialTime, Index, Interval) ->
    InitialTime + (Index * Interval).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

divide_array_test() ->
    ?assertEqual({[], []}, divide_array(array:new(), 4)),
    ?assertEqual(
        {[1, 2], [3, 4]},
        divide_array(array:from_list([1, 2, 3, 4]), 2)
    ),
    ?assertEqual(
        {[1, 2, 3, 4], []},
        divide_array(array:from_list([1, 2, 3, 4]), 7)
    ).

calculate_starttime_test() ->
    ?assertEqual(2, calculate_starttime(3, [{2, 2}, {4, 2}, {6, 2}, {8, 2}])),
    ?assertEqual(2, calculate_starttime(1, [{2, 2}, {4, 2}, {6, 2}, {8, 2}])).

calculate_endtime_test() ->
    ?assertEqual(4, calculate_endtime(3, [{2, 2}, {4, 2}, {6, 2}, {8, 2}])),
    ?assertEqual(9, calculate_endtime(9, [{2, 2}, {4, 2}, {6, 2}, {8, 2}])).

chunkify_test() ->
    %% No chunking
    Input = [[1], [2], [3, 5]],
    ?assertEqual(
        {Input, []},
        chunkify(
            Input,
            fun oscilloscope_cache_aggregations:avg/1,
            1000000,
            1000000
        )
    ),
    %% Chunking each value
    {Remainder, Chunked} = chunkify(
        Input,
        fun oscilloscope_cache_aggregations:avg/1,
        0,
        1000000
    ),
    Decoded = lists:map(fun({I, V, _}) -> {I, ?VALDECODE(V)} end, Chunked),
    ?assertEqual([], Remainder),
    ?assertEqual([{0, [1.0]}, {1, [2.0]}, {2, [4.0]}], Decoded),
    %% Chunking multiple values together
    Input1 = [[float(I)] || I <- lists:seq(0, 21)],
    {Remainder1, Chunked1} = chunkify(
        Input1,
        fun oscilloscope_cache_aggregations:avg/1,
        50,
        75
    ),
    Decoded1 = lists:map(fun({I, V, _}) -> {I, ?VALDECODE(V)} end, Chunked1),
    ?assertEqual([], Remainder1),
    ?assertEqual([
        {0, [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0]},
        {8, [8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0]},
        {15, [15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0]}
    ], Decoded1).

process_null_test() ->
    T0 = undefined,
    Points = array:new({default, null}),
    Interval = 10,
    Persisted = [],
    Timestamp = 12345,
    Value = 42,
    MinPersistAge = 300,
    {T, P} = process(
        Timestamp,
        Value,
        T0,
        Points,
        Interval,
        Persisted,
        MinPersistAge
    ),
    ?assertEqual(12340, T),
    ?assertEqual([[42]], array:to_list(P)).

process_one_test() ->
    T0 = 50,
    Points = array:from_list([[1]], null),
    Interval = 10,
    Persisted = [],
    Timestamp = 62,
    Value = 42,
    MinPersistAge = 300,
    {T, P} = process(
        Timestamp,
        Value,
        T0,
        Points,
        Interval,
        Persisted,
        MinPersistAge
    ),
    ?assertEqual(50, T),
    ?assertEqual([[1], [42]], array:to_list(P)).

process_skip_test() ->
    T0 = 50,
    Points = array:from_list([[1]], null),
    Interval = 10,
    Persisted = [],
    Timestamp = 72,
    Value = 42,
    MinPersistAge = 300,
    {T, P} = process(
        Timestamp,
        Value,
        T0,
        Points,
        Interval,
        Persisted,
        MinPersistAge
    ),
    ?assertEqual(50, T),
    ?assertEqual([[1], null, [42]], array:to_list(P)).

process_negative_accept_test() ->
    T0 = 50,
    Points = array:from_list([[1]]),
    Interval = 10,
    Persisted = [],
    Timestamp = 32,
    Value = 40,
    MinPersistAge = 300,
    {T, P} = process(
        Timestamp,
        Value,
        T0,
        Points,
        Interval,
        Persisted,
        MinPersistAge
    ),
    ?assertEqual(30, T),
    ?assertEqual([[40], null, [1]], array:to_list(P)).

process_negative_reject_test() ->
    MinPersistAge = 300,
    T0 = 50000,
    Points = array:from_list([[1]]),
    Interval = 10,
    Persisted = [],
    Timestamp = T0 - MinPersistAge - Interval,
    Value = 40,
    MinPersistAge = 300,
    {T, P} = process(
        Timestamp,
        Value,
        T0,
        Points,
        Interval,
        Persisted,
        MinPersistAge
    ),
    ?assertEqual(T0, T),
    ?assertEqual(Points, P).

append_point_test() ->
    DP0 = array:new({default, null}),
    DP1 = append_point(0, 45, DP0),
    ?assertEqual([[45]], array:to_list(DP1)),
    DP2 = append_point(0, 50, DP1),
    ?assertEqual([[50, 45]], array:to_list(DP2)),
    DP3 = append_point(2, 42, DP2),
    ?assertEqual([[50, 45], null, [42]], array:to_list(DP3)).

cached_read_null_test() ->
    Result = cached_read(
        100,
        200,
        20,
        fun oscilloscope_cache_aggregations:avg/1,
        undefined,
        array:new({default, null})
    ),
    ?assertEqual(lists:duplicate(5, null), Result).

cached_read_some_end_test() ->
    Points = [[20.0] || _ <- lists:seq(1, 10)],
    Result = cached_read(
        100,
        200,
        20,
        fun oscilloscope_cache_aggregations:avg/1,
        140,
        array:from_list(Points, null)
    ),
    Expected = [null, null, 20.0, 20.0, 20.0, 20.0],
    ?assertEqual(Expected, Result).

cached_read_some_start_test() ->
    Points = [[20.0] || _ <- lists:seq(1, 3)],
    Result = cached_read(
        100,
        200,
        20,
        fun oscilloscope_cache_aggregations:avg/1,
        100,
        array:from_list(Points, null)
    ),
    Expected = [20.0, 20.0, 20.0, null, null, null],
    ?assertEqual(Expected, Result).

cached_read_middle_test() ->
    Points = [[20.0], [20.0]],
    Result = cached_read(
        12330,
        12360,
        10,
        fun oscilloscope_cache_aggregations:avg/1,
        12340,
        array:from_list(Points, null)
    ),
    Expected = [null, 20.0, 20.0, null],
    ?assertEqual(Expected, Result).

cached_read_all_test() ->
    Points = [[20.0] || _ <- lists:seq(1, 10)],
    Result = cached_read(
        100,
        200,
        20,
        fun oscilloscope_cache_aggregations:avg/1,
        80,
        array:from_list(Points, null)
    ),
    Expected = [20.0, 20.0, 20.0, 20.0, 20.0, 20.0],
    ?assertEqual(Expected, Result).

calculate_query_bounds_test() ->
    ?assertEqual({10, 20}, calculate_query_bounds(12, 17, 10)),
    ?assertEqual({10, 20}, calculate_query_bounds(10, 20, 10)),
    ?assertEqual({10, 20}, calculate_query_bounds(12, 11, 10)).

select_pid_for_query_test() ->
    Metadata = [
        {a, {ok, [{interval, 10}, {earliest_time, 500}]}},
        {b, {ok, [{interval, 20}, {earliest_time, 200}]}},
        {c, {ok, [{interval, 30}, {earliest_time, 100}]}}
    ],
    ?assertEqual(a, select_pid_for_query(Metadata, 500)),
    ?assertEqual(a, select_pid_for_query(Metadata, 550)),
    ?assertEqual(b, select_pid_for_query(Metadata, 450)),
    ?assertEqual(c, select_pid_for_query(Metadata, 0)).

maybe_trim_points_test() ->
    ?assertEqual(
        {100, array:from_list([a, b, c, d], null)},
        maybe_trim_points(100, array:from_list([a, b, c, d], null), 10, 20)
    ),
    ?assertEqual(
        {120, array:from_list([c, d], null)},
        maybe_trim_points(100, array:from_list([a, b, c, d], null), 10, 2)
    ).

-endif.
