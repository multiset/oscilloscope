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
    commutator
}).

start() ->
    application:start(oscilloscope_cache).

stop() ->
    ok.

process(User, Name, Host, Timestamp, Value) ->
    multicast(User, Name, Host, {process, Timestamp, Value}).

read(User, Name, Host, From, Until) ->
    Resolutions = multicall(User, Name, Host, get_metadata),
    [{Pid0, _}|Rs] = lists:sort(
        fun({_, {ok, MetaA}}, {_, {ok, MetaB}}) ->
            IntervalA = proplists:get_value(interval, MetaA),
            IntervalB = proplists:get_value(interval, MetaB),
            IntervalA >= IntervalB
        end,
        Resolutions
    ),
    Pid = lists:foldl(
        fun({P, {ok, Meta}}, Acc) ->
            case proplists:get_value(earliest_time, Meta) < From of
                true -> P;
                false -> Acc
            end
        end, Pid0, Rs),
    gen_server:call(Pid, {read, From, Until}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init({Group, ResolutionId, Interval, Count, Persisted, AggregationAtom}) ->
    ok = pg2:join(Group, self()),
    AggregationFun = fun(Vals) ->
        erlang:apply(oscilloscope_cache_aggregations, AggregationAtom, [Vals])
    end,
    {ok, C} = commutator:connect(
        ?DYNAMO_TABLE,
        ?DYNAMO_SCHEMA,
        ?DYNAMO_REGION,
        ?DYNAMO_ACCESSKEY,
        ?DYNAMO_SECRETKEY
    ),
    State = #st{
        resolution_id = ResolutionId,
        group = Group,
        interval = Interval,
        count = Count,
        persisted = Persisted,
        aggregation_fun = AggregationFun,
        commutator = C
    },
    case oscilloscope_cache_memory:read(State#st.resolution_id) of
        not_found ->
            %% TODO: get T0 (undefined here) from persistent store
            oscilloscope_cache_memory:write(
                State#st.resolution_id,
                {undefined, array:new({default, null})}
            );
        _ ->
            ok
    end,
    {ok, State}.

handle_call(get_metadata, _From, #st{interval=Interval, count=Count}=State) ->
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
        {latest_time, LatestTime},
        {earliest_time, EarliestTime},
        {interval, Interval},
        {count, Count},
        {aggregation_fun, State#st.aggregation_fun}
    ],
    {reply, {ok, Metadata}, State};
handle_call({read, From, Until}, _From, State) ->
    #st{
        resolution_id=Id,
        interval=Interval,
        persisted=Persisted,
        aggregation_fun=AF,
        commutator=Commutator
    } = State,
    {T0, Points} = oscilloscope_cache_memory:read(State#st.resolution_id),
    RealFrom = From - (From rem Interval),
    RealUntil = Until + Interval - (Until rem Interval),
    Cached = cached_read(RealFrom, RealUntil, Interval, AF, T0, Points),
    Points = case T0 >= RealFrom of
        false ->
            Cached;
        true ->
            persistent_read(Id, Commutator, From, Until, Persisted) ++ Cached
    end,
    Reply = [
        {from, From},
        {until, Until},
        {interval, Interval},
        {datapoints, Points}
    ],
    {reply, {ok, Reply}, State};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, error, State}.

handle_cast({process, Timestamp, Value}, State) ->
    Key = State#st.resolution_id,
    {T0, ExistingPoints} = oscilloscope_cache_memory:read(Key),
    {T1, NewPoints} = process(
        Timestamp,
        Value,
        T0,
        ExistingPoints,
        State#st.interval,
        State#st.persisted
    ),
    oscilloscope_cache_memory:write(Key, {T1, NewPoints}),
    {noreply, State, 0};
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(timeout, State) ->
    #st{
        resolution_id=Id,
        interval=Interval,
        persisted=Persisted,
        aggregation_fun=AF,
        commutator=Commutator
    } = State,
    {T0, Points} = oscilloscope_cache_memory:read(State#st.resolution_id),
    {ToPersist, T1, ToCache} = split_for_persisting(T0, Points, Interval, AF),
    TimestampsPersisted = persist(ToPersist, Id, Commutator),
    oscilloscope_cache_memory:write(State#st.resolution_id, {T1, ToCache}),
    {noreply, State#st{persisted=Persisted ++ TimestampsPersisted}};
handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

process(Timestamp, Value, T0, Points, Interval, Persisted) ->
    %% Timestamps are always floored to fit intervals exactly
    Timestamp1 = Timestamp - (Timestamp rem Interval),
    %% We claim a ?MIN_PERSIST_AGE maximum age
    %% but that's enforced by not persisting newer points.
    %% In practice we'll accept anything that's newer than the last persist.
    LastPersist = case Persisted of
        [] -> 0;
        Persisted -> lists:last(Persisted)
    end,
    case Timestamp1 > LastPersist of
        true ->
            case T0 of
                undefined ->
                    {Timestamp1, append_point(0, Value, Points)};
                T when T =< Timestamp1 ->
                    Index = (Timestamp1 - T0) div Interval,
                    {T0, append_point(Index, Value, Points)};
                T when T - ?MIN_PERSIST_AGE =< Timestamp1 ->
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

cached_read(From, Until, Interval, AF, T, Points) when T =< From ->
    %% The full query result is in the cache
    StartIndex = (From - T) div Interval,
    EndIndex = (Until - T) div Interval,
    Result = range_from_array(StartIndex, EndIndex, AF, [], Points),
    %% But the cache might not have all the points we need
    Missing = erlang:trunc((((Until - From) / Interval) + 1) - length(Result)),
    Result ++ lists:duplicate(Missing, null);
cached_read(From, Until, Interval, AF, T, Points) when T =< Until ->
    %% Part of the query is in the cache
    OutOfRange = (T - From) div Interval,
    Acc0 = lists:duplicate(OutOfRange, null),
    EndIndex = (Until - T) div Interval,
    range_from_array(0, EndIndex, AF, Acc0, Points);
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

split_for_persisting(T0, Points, Interval, AggregationFun) ->
    %% Only persist up to the newest point - ?MIN_PERSIST_AGE
    PersistIndex = erlang:trunc(array:size(Points) - ?MIN_PERSIST_AGE/Interval),
    case divide_array(Points, PersistIndex) of
        {[], _} ->
            {[], T0, Points};
        {ToPersist, ToMem} ->
            %% Only persist chunks that are large enough
            {Remainder, ChunksToPersist} = chunkify(
                ToPersist,
                AggregationFun,
                ?MIN_CHUNK_SIZE,
                ?MAX_CHUNK_SIZE
            ),
            ChunksWithTimestamps = lists:map(
                fun({I, V}) -> {timestamp_from_index(T0, I, Interval), V} end,
                ChunksToPersist
            ),
            %% Merge extras back with the points to remain in memory and
            %% increment the start timestamp for the number of points persisted
            PersistCount = length(ToPersist) - length(Remainder),
            T1 = timestamp_from_index(T0, PersistCount, Interval),
            {ChunksWithTimestamps, T1, Remainder ++ ToMem}
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
        fun({Timestamp, Value}) ->
            {ok, true} = commutator:put_item(
                Commutator,
                [{<<"id">>, Id}, {<<"t">>, Timestamp}, {<<"v">>, Value}]
            ),
            ok = oscilloscope_sql_metrics:update_persisted(Id, Timestamp),
            Timestamp
        end, Points
    ).

persistent_read(_Id, _Commutator, _From, _Until, []) ->
    [];
persistent_read(Id, Commutator, From, Until, Persisted) ->
    StartTime = calculate_starttime(From, Persisted),
    EndTime = calculate_endtime(Until, Persisted),
    {ok, Rows} = commutator:query(
        Commutator,
        [{<<"id">>, eq, [Id]}, {<<"t">>, between, [StartTime, EndTime]}],
        100000 %% TODO: paginate, and teach commutator to accept no limit
    ),
    lists:flatten([?VALDECODE(proplists:get_value(<<"v">>, I)) || I <- Rows]).

calculate_starttime(T, Ts) ->
    {Earlier, Later} = lists:partition(fun(I) -> I =< T end, Ts),
    case Earlier of
        [] -> hd(Later);
        _Any -> lists:last(Earlier)
    end.

calculate_endtime(T, Ts) ->
    {Later, _Earlier} = lists:partition(fun(I) -> I >= T end, Ts),
    case Later of
        [] -> T;
        _Any -> hd(Later)
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
                    {Count + length(Pending1), [], [{Count, Encoded}|Chunks]};
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

multicast(User, Name, Host, Msg) ->
    Pids = get_pids(User, Name, Host),
    lists:map(fun(P) -> gen_server:cast(P, Msg) end, Pids).

multicall(User, Name, Host, Msg) ->
    Pids = get_pids(User, Name, Host),
    lists:map(fun(P) -> {P, gen_server:call(P, Msg)} end, Pids).

get_pids(User, Name, Host) ->
    Group = {User, Name, Host},
    case pg2:get_members(Group) of
        {error, {no_such_group, Group}} ->
            {ok, _Pid} = oscilloscope_cache_sup:spawn_cache(Group),
            pg2:get_members(Group);
        [] ->
            {ok, _Pid} = oscilloscope_cache_sup:spawn_cache(Group),
            pg2:get_members(Group);
        P ->
            P
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
    ?assertEqual(2, calculate_starttime(3, [2, 4, 6, 8])),
    ?assertEqual(2, calculate_starttime(1, [2, 4, 6, 8])).

calculate_endtime_test() ->
    ?assertEqual(4, calculate_endtime(3, [2, 4, 6, 8])),
    ?assertEqual(9, calculate_endtime(9, [2, 4, 6, 8])).

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
    Decoded = lists:map(fun({I, V}) -> {I, ?VALDECODE(V)} end, Chunked),
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
    Decoded1 = lists:map(fun({I, V}) -> {I, ?VALDECODE(V)} end, Chunked1),
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
    {T, P} = process(Timestamp, Value, T0, Points, Interval, Persisted),
    ?assertEqual(12340, T),
    ?assertEqual([[42]], array:to_list(P)).

process_one_test() ->
    T0 = 50,
    Points = array:from_list([[1]], null),
    Interval = 10,
    Persisted = [],
    Timestamp = 62,
    Value = 42,
    {T, P} = process(Timestamp, Value, T0, Points, Interval, Persisted),
    ?assertEqual(50, T),
    ?assertEqual([[1], [42]], array:to_list(P)).

process_skip_test() ->
    T0 = 50,
    Points = array:from_list([[1]], null),
    Interval = 10,
    Persisted = [],
    Timestamp = 72,
    Value = 42,
    {T, P} = process(Timestamp, Value, T0, Points, Interval, Persisted),
    ?assertEqual(50, T),
    ?assertEqual([[1], null, [42]], array:to_list(P)).

process_negative_accept_test() ->
    T0 = 50,
    Points = array:from_list([[1]]),
    Interval = 10,
    Persisted = [],
    Timestamp = 32,
    Value = 40,
    {T, P} = process(Timestamp, Value, T0, Points, Interval, Persisted),
    ?assertEqual(30, T),
    ?assertEqual([[40], null, [1]], array:to_list(P)).

process_negative_reject_test() ->
    T0 = 50000,
    Points = array:from_list([[1]]),
    Interval = 10,
    Persisted = [],
    Timestamp = T0 - ?MIN_PERSIST_AGE - Interval,
    Value = 40,
    {T, P} = process(Timestamp, Value, T0, Points, Interval, Persisted),
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

-endif.
