-module(oscilloscope_cache).

-export([
    start/0,
    stop/0,
    process/3,
    read/3
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
    time :: pos_integer() | undefined,
    points :: array(),
    metric_id :: metric_id(),
    resolution_id :: resolution_id(),
    interval :: interval(),
    count :: count(),
    min_persist_age :: pos_integer(),
    persisted :: persisted(),
    aggregation_fun :: fun(),
    persisting :: pid() | nil,
    vacuuming :: pid() | nil,
    readers :: [pid()]
}).

start() ->
    application:start(oscilloscope_cache).

stop() ->
    ok.

-spec process(metric(), timestamp(), float()) -> any().
process(Metric, Timestamp, Value) ->
    lager:debug(
        "Processing point: ~p ~p ~p",
        [Metric, Timestamp, Value]
    ),
    multicast(Metric, {process, [{Timestamp, Value}]}).

-spec read(metric(), timestamp(), timestamp()) -> {ok, [{_, _}]}.
read(Metric, From, Until) ->
    lager:debug(
        "Processing read: ~p ~p ~p",
        [Metric, From, Until]
    ),
    CacheMetadata = multicall(Metric, get_metadata),
    Pid = select_pid_for_query(CacheMetadata, From),
    gen_server:call(Pid, {read, From, Until}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    {
        MetricId,
        ResolutionId,
        Interval,
        Count,
        MinPersistAge,
        Persisted,
        AggregationAtom
    } = Args,
    process_flag(trap_exit, true),
    lager:info(
        "Booting cache pid ~p for interval ~p, count ~p",
        [self(), Interval, Count]
    ),
    folsom_metrics:notify({oscilloscope_cache, cache_inits}, {inc, 1}),
    AggregationFun = fun(Vals) ->
        erlang:apply(oscilloscope_cache_aggregations, AggregationAtom, [Vals])
    end,
    State = #st{
        time = undefined,
        points = array:new({default, null}),
        metric_id = MetricId,
        resolution_id = ResolutionId,
        interval = Interval,
        count = Count,
        min_persist_age = MinPersistAge,
        persisted = Persisted,
        aggregation_fun = AggregationFun,
        persisting = nil,
        vacuuming = nil,
        readers = []
    },
    {ok, State}.

handle_call(get_metadata, _From, State) ->
    #st{
        time=T0,
        points=Points,
        interval=Interval,
        count=Count,
        persisted=Persisted
    } = State,
    {EarliestCache, LatestCache} = case T0 of
        undefined ->
            {undefined, undefined};
        _ ->
            {T0, timestamp_from_index(T0, array:size(Points) - 1, Interval)}
    end,
    {EarliestTime, LatestTime} = case Persisted of
        [] ->
            {EarliestCache, LatestCache};
        [{P0, _}|_] ->
            case LatestCache of
                undefined ->
                    %% No data in the cache, return the bounds of persisted data
                    {TimeN, CountN} = lists:last(Persisted),
                    {P0, timestamp_from_index(TimeN, CountN, Interval)};
                _ ->
                    {P0, LatestCache}
            end
    end,
    Metadata = [
        {earliest_time, EarliestTime},
        {latest_time, LatestTime},
        {interval, Interval},
        {count, Count},
        {aggregation_fun, State#st.aggregation_fun}
    ],
    {reply, {ok, Metadata}, State};
handle_call(get_cached, _From, #st{time=T, points=Points}=State) ->
    {reply, {ok, {T, array:to_list(Points)}}, State};
handle_call({read, From0, Until0}, _From, State) when From0 > Until0 ->
    {reply, {error, temporal_inversion}, State};
handle_call({read, From, Until}, Client, #st{readers=Readers}=State) ->
    ReaderPid = spawn_link(fun() -> async_read(From, Until, Client, State) end),
    {noreply, State#st{readers=[ReaderPid|Readers]}};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, error, State}.

handle_cast({process, IncomingPoints}, State) ->
    #st{
        time=T0,
        points=Points0,
        interval=Interval,
        count=Count,
        persisted=Persisted
    } = State,
    {T1, Points1} = process(
        IncomingPoints,
        T0,
        Points0,
        Interval,
        Persisted
    ),
    {T2, Points2} = maybe_trim_points(
        T1,
        Points1,
        Interval,
        Count
    ),
    {noreply, State#st{time=T2, points=Points2}, 0};
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(timeout, #st{persisting=nil, vacuuming=nil}=State) ->
    #st{
        resolution_id=Id,
        time=T,
        points=Points,
        persisted=Persisted,
        aggregation_fun=AF,
        interval=Interval,
        count=Count,
        min_persist_age=MinPersistAge
    } = State,
    PersistIndex = erlang:trunc(array:size(Points) - MinPersistAge / Interval),
    {PersistCandidates, _} = divide_array(Points, PersistIndex),
    {_, AggregatedPersistCandidates} = lists:foldr(
        fun(Values, {N, Acc}) ->
            {N + 1, [{timestamp_from_index(T, N, Interval), AF(Values)}|Acc]}
        end,
        {0, []},
        PersistCandidates
    ),
    PersistPid = case AggregatedPersistCandidates of
        [] ->
            nil;
        _ ->
            spawn_link(
                fun() ->
                    exit(oscilloscope_persistence:persist(
                        Id,
                        AggregatedPersistCandidates
                    ))
                end
            )
    end,
    TNow = timestamp_from_index(T, array:size(Points), Interval),
    TExpired = TNow - Interval * Count,
    VacuumCandidates = [Time || {Time, _} <- Persisted, Time < TExpired],
    VacuumPid = case VacuumCandidates of
        [] ->
            nil;
        _ ->
            spawn_link(
                fun() ->
                    exit(oscilloscope_persistence:vacuum(Id, VacuumCandidates))
                end
            )
    end,
    {noreply, State#st{persisting=PersistPid, vacuuming=VacuumPid}};
handle_info(timeout, State) ->
    %% Persists and/or vacuums are still outstanding
    {noreply, State};
handle_info({'EXIT', From, Response}, #st{persisting=From}=State) ->
    #st{
        resolution_id = Id,
        time = T0,
        points = Points0,
        interval = Interval
    } = State,
    {T1, Points1} = case Response of
        {ok, []} ->
            {T0, Points0};
        {ok, Persisted} ->
            trim_persisted_points(Persisted, Points0, Interval);
        Error ->
            lager:error(
                "Persist attempt for cache id ~p failed: ~p",
                [Id, Error]
            ),
            {T0, Points0}
    end,
    {noreply, State#st{time=T1, points=Points1, persisting=nil}};
handle_info({'EXIT', From, Response}, #st{vacuuming=From}=State) ->
    #st{
        resolution_id = Id,
        persisted = Persisted0
    } = State,
    Persisted1 = case Response of
        {ok, Vacuumed} ->
            lists:filter(
                fun({Time, _}) -> lists:member(Time, Vacuumed) =:= false end,
                Persisted0
            );
        Error ->
            lager:error(
                "Vacuum attempt for cache id ~p failed: ~p",
                [Id, Error]
            ),
            Persisted0
    end,
    {noreply, State#st{persisted=Persisted1, vacuuming=nil}};
handle_info({'EXIT', From, Response}, State) ->
    #st{
        resolution_id = Id,
        readers = Readers
    } = State,
    Readers1 = case lists:delete(From, Readers) of
        Readers ->
            lager:error(
                "Cache ~p received EXIT from unknown linked pid", [Id]
            ),
            Readers;
        Else ->
            Else
    end,
    {noreply, State#st{readers=Readers1}};
handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

async_read(From0, Until0, Client, #st{}=State) ->
    #st{
        time=T0,
        points=Points,
        resolution_id=Id,
        interval=Interval,
        persisted=Persisted,
        aggregation_fun=AF
    } = State,
    folsom_metrics:notify({oscilloscope_cache, reads}, {inc, 1}),
    {From, Until} = calculate_query_bounds(From0, Until0, Interval),
    Cached = cached_read(From, Until, Interval, AF, T0, Points),
    Data = case T0 > From of
        false ->
            Cached;
        true ->
            Disk = persistent_read(
                Id,
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
    gen_server:reply(Client, {ok, Reply}).

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
        end,
        Pid0,
        Ms
    ).

process([], T, Points, _Interval, _Persisted) ->
    {T, Points};
process([P|Ps], T0, Points0, Interval, Persisted) ->
    {Timestamp0, Value} = P,
    %% Timestamps are always floored to fit intervals exactly
    Timestamp1 = Timestamp0 - (Timestamp0 rem Interval),
    %% Any point that's newer than the last persist time is acceptable, but
    %% we'll never try to overwrite a previously-persisted index.
    LastPersist = case Persisted of
        [] ->
            -1;
        Persisted ->
            {LastPersistTime, LastPersistCount} = lists:last(Persisted),
            LastPersistTime + Interval * LastPersistCount
    end,
    {T1, Points1} = case {Timestamp1 > LastPersist, T0} of
        {false, _} ->
            {T0, Points0};
        {true, undefined} ->
            {Timestamp1, append_point(0, Value, Points0)};
        {true, T0} when T0 =< Timestamp1 ->
            Index = (Timestamp1 - T0) div Interval,
            {T0, append_point(Index, Value, Points0)};
        {true, _} ->
            %% Need to slide the window backwards - this is a legal
            %% point that's at a negative index in the current array.
            IndexesToAdd = (T0 - Timestamp1) div Interval,
            {Timestamp1, prepend_point(IndexesToAdd, Value, Points0)}
    end,
    process(Ps, T1, Points1, Interval, Persisted).

maybe_trim_points(undefined, Points, _Interval, _Count) ->
    {undefined, Points};
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
        end,
        Acc0,
        Points
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

persistent_read(_Id, _From, _Until, _Interval, []) ->
    [];
persistent_read(Id, From, Until, Interval, Persisted) ->
    StartTime = calculate_starttime(From, Persisted),
    EndTime = calculate_endtime(Until, Persisted),
    {ok, Points} = oscilloscope_persistence:read(Id, StartTime, EndTime),
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

trim_persisted_points(Persisted, Points0, Interval) ->
    {Timestamp, Size} = lists:last(Persisted),
    T = Timestamp + (Interval * Size),
    TotalPersisted = lists:foldl(
        fun({_, S}, Acc) -> S + Acc end,
        0,
        Persisted
    ),
    {_, PointsList} = divide_array(Points0, TotalPersisted),
    Points1 = array:from_list(PointsList, null),
    {T, Points1}.

multicast(Metric, Msg) ->
    Pids = get_pids(Metric),
    lists:map(fun(P) -> gen_server:cast(P, Msg) end, Pids).

multicall(Metric, Msg) ->
    Pids = get_pids(Metric),
    lists:map(fun(P) -> {P, gen_server:call(P, Msg)} end, Pids).

get_pids(Metric) ->
    case oscilloscope_cache_sup:find_group(Metric) of
        not_found ->
            oscilloscope_cache_sup:spawn_group(Metric);
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

process_null_test() ->
    T0 = undefined,
    Points = array:new({default, null}),
    Interval = 10,
    Persisted = [],
    Timestamp = 12345,
    Value = 42,
    {T, P} = process(
        [{Timestamp, Value}],
        T0,
        Points,
        Interval,
        Persisted
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
    {T, P} = process(
        [{Timestamp, Value}],
        T0,
        Points,
        Interval,
        Persisted
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
    {T, P} = process(
        [{Timestamp, Value}],
        T0,
        Points,
        Interval,
        Persisted
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
    {T, P} = process(
        [{Timestamp, Value}],
        T0,
        Points,
        Interval,
        Persisted
    ),
    ?assertEqual(30, T),
    ?assertEqual([[40], null, [1]], array:to_list(P)).

process_negative_reject_test() ->
    T0 = 50000,
    Points = array:from_list([[1]]),
    Interval = 10,
    Persisted = [{40000, 12}],
    Timestamp = 30000,
    Value = 40,
    {T, P} = process(
        [{Timestamp, Value}],
        T0,
        Points,
        Interval,
        Persisted
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

trim_persisted_points_test() ->
    Points = array:from_list([[20.0] || _ <- lists:seq(1, 10)], null),
    Persisted = [{10, 1}, {20, 1}, {30, 1}],
    Interval = 10,
    Output = {40, array:from_list([[20.0] || _ <- lists:seq(1, 7)], null)},
    ?assertEqual(Output, trim_persisted_points(Persisted, Points, Interval)).

-endif.
