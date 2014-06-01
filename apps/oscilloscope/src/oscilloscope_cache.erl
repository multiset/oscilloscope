-module(oscilloscope_cache).

-export([
    new/1,
    refresh/1,
    update/2,
    read/3,
    cached/2,
    fold/3
]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

-record(cache, {
    metric,
    meta,
    aggregation,
    resolutions
}).

-record(resolution, {
    meta :: oscilloscope_metadata_resolution:resolution(),
    t :: timestamp(), %% Earliest point in array
    points :: array() %% Array of points
}).

-ifdef(TEST).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-endif.

new(Metric) ->
    Meta = case oscilloscope_metadata:find(Metric) of
        {ok, M} ->
            M;
        {error, not_found} ->
            {ok, M} = oscilloscope_metadata:create(Metric),
            M
    end,
    AggregationAtom = oscilloscope_metadata:aggregation(Meta),
    AggregationFun = fun(Vals) ->
        erlang:apply(oscilloscope_aggregations, AggregationAtom, [Vals])
    end,
    Resolutions = lists:map(
        fun(R) ->
            #resolution{
                meta=R,
                t=undefined,
                points=array:new({default, null})
            }
        end,
        oscilloscope_metadata:resolutions(Meta)
    ),
    #cache{
        metric=Metric,
        meta=Meta,
        aggregation=AggregationFun,
        resolutions=Resolutions
    }.

refresh(Cache) ->
    #cache{
        metric=Metric,
        meta=Meta0,
        aggregation=AF0,
        resolutions=Resolutions0
    } = Cache,
    {ok, Meta1} = oscilloscope_metadata:find(Metric),
    Aggregation0 = oscilloscope_metadata:aggregation(Meta0),
    Aggregation1 = oscilloscope_metadata:aggregation(Meta1),
    AF1 = case Aggregation0 == Aggregation1 of
        true ->
            AF0;
        false ->
            fun(Vals) ->
                erlang:apply(oscilloscope_aggregations, Aggregation1, [Vals])
            end
    end,
    NewResolutions = oscilloscope_metadata:resolutions(Meta1),
    %% TODO: Handle addition or removal of resolutions
    Resolutions1 = lists:map(
        fun(R) -> refresh_resolution(R, NewResolutions) end,
        Resolutions0
    ),
    Cache#cache{aggregation=AF1, resolutions=Resolutions1}.

update(Points, Cache) ->
    #cache{resolutions=Resolutions0} = Cache,
    Resolutions1 = lists:map(
        fun(Resolution) -> update_int(Points, Resolution) end,
        Resolutions0
    ),
    Cache#cache{resolutions=Resolutions1}.

read(From, Until, _Cache) when From > Until ->
    {error, temporal_inversion};
read(From0, Until0, Cache) ->
    #cache{resolutions=Resolutions, aggregation=Aggregation} = Cache,
    #resolution{meta=Meta, t=T, points=Points} = select_resolution(
        From0,
        Resolutions
    ),
    Interval = oscilloscope_metadata_resolution:interval(Meta),
    {From1, Until1, Read} = read_int(
        From0,
        Until0,
        Interval,
        Aggregation,
        T,
        Points
    ),
    {From1, Until1, Meta, Read}.

-spec cached(Cache, Resolution) -> {Points, Meta} when
    Cache :: #cache{},
    Resolution :: #resolution{},
    Points :: [{timestamp(), number()}],
    Meta :: oscilloscope_metadata_resolution:resolution().

cached(Cache, Resolution) ->
    #cache{aggregation=AF} = Cache,
    #resolution{meta=Meta, t=T, points=PointsArray} = Resolution,
    Interval = oscilloscope_metadata_resolution:interval(Meta),
    Points = array:foldr(
        fun(Idx, Values, Acc) -> [{T + (Idx * Interval), AF(Values)}|Acc] end,
        [],
        PointsArray
    ),
    {Points, Meta}.

fold(Fun, Acc, Cache) ->
    #cache{resolutions=Resolutions} = Cache,
    lists:foldl(fun(R) -> Fun(Cache, R, Acc) end, Acc, Resolutions).

refresh_resolution(Resolution, Metas) ->
    %% TODO: This only handles persistence updates
    #resolution{meta=Meta0, t=T0, points=Points0} = Resolution,
    ID = oscilloscope_metadata_resolution:id(Meta0),
    Interval = oscilloscope_metadata_resolution:interval(Meta0),
    [Meta1] = lists:filter(
        fun(R) -> oscilloscope_metadata_resolution:id(R) == ID end,
        Metas
    ),
    LatestPersist = oscilloscope_metadata_resolution:latest_persist_time(Meta1),
    {T1, Points1} = case LatestPersist of
        undefined ->
            {T0, Points0};
        T0 ->
            {T0, Points0};
        Else ->
            %% TODO: probably an off-by-one here
            Index = (Else - T0) div Interval,
            {_, PointsList} = divide_array(Points0, Index),
            {Else + Interval, array:from_list(PointsList, null)}
    end,
    #resolution{meta=Meta1, t=T1, points=Points1}.

update_int(Points, Resolution) ->
    #resolution{meta=Meta, t=T0, points=Points0} = Resolution,
    Interval = oscilloscope_metadata_resolution:interval(Meta),
    Count = oscilloscope_metadata_resolution:count(Meta),
    Persisted = oscilloscope_metadata_resolution:persisted(Meta),
    {T1, Points1} = append(Points, T0, Points0, Interval, Persisted),
    {T2, Points2} = maybe_trim(T1, Points1, Interval, Count),
    Resolution#resolution{t=T2, points=Points2}.

append([], T, Points, _Interval, _Persisted) ->
    {T, Points};
append([{Timestamp0, Value}|Ps], T0, Points0, Interval, Persisted) ->
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
    append(Ps, T1, Points1, Interval, Persisted).

maybe_trim(undefined, Points, _Interval, _Count) ->
    {undefined, Points};
maybe_trim(T, Points0, Interval, Count) ->
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

prepend_point(Index, Value, Points) ->
    ListPoints = array:to_list(Points),
    Prepend = lists:duplicate(Index, null),
    Points1 = array:from_list(Prepend ++ ListPoints, null),
    append_point(0, Value, Points1).

append_point(Index, Value, Points) when Index >= 0 ->
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

select_resolution(From, Resolutions) ->
    [R|Rs] = lists:sort(
        fun(#resolution{meta=MetaA}, #resolution{meta=MetaB}) ->
            IntervalA = oscilloscope_metadata_resolution:interval(MetaA),
            IntervalB = oscilloscope_metadata_resolution:interval(MetaB),
            IntervalA >= IntervalB
        end,
        Resolutions
    ),
    lists:foldl(
        fun(Resolution, Selected) ->
            case earliest_timestamp(Resolution) =< From of
                true -> Resolution;
                false -> Selected
            end
        end,
        R,
        Rs
    ).

earliest_timestamp(Resolution) ->
    #resolution{meta=Meta, t=T} = Resolution,
    case oscilloscope_metadata_resolution:earliest_persist_time(Meta) of
        undefined -> T;
        Timestamp -> Timestamp
    end.

read_int(From0, Until0, Interval, Aggregation, T, Points) ->
    {From, Until} = calculate_query_bounds(From0, Until0, Interval),
    case T =< Until of
        true ->
            %% At least some of the query is in the cache
            Acc0 = case (T - From) div Interval of
                Count when Count > 0 -> lists:duplicate(Count, null);
                _ -> []
            end,
            StartIndex = (From - T) div Interval,
            EndIndex = (Until - T) div Interval,
            Result = range_from_array(
                StartIndex,
                EndIndex,
                Aggregation,
                Acc0,
                Points
            ),
            %% But the cache might not have all the points we need
            Missing = trunc((((Until - From) / Interval) + 1) - length(Result)),
            {From, Until, Result ++ lists:duplicate(Missing, null)};
        false ->
            PointCount = ((Until - From) div Interval + 1),
            {From, Until, lists:duplicate(PointCount, null)}
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

-ifdef(TEST).

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

append_null_test() ->
    T0 = undefined,
    Points = array:new({default, null}),
    Interval = 10,
    Persisted = [],
    Timestamp = 12345,
    Value = 42,
    {T, P} = append(
        [{Timestamp, Value}],
        T0,
        Points,
        Interval,
        Persisted
    ),
    ?assertEqual(12340, T),
    ?assertEqual([[42]], array:to_list(P)).

append_one_test() ->
    T0 = 50,
    Points = array:from_list([[1]], null),
    Interval = 10,
    Persisted = [],
    Timestamp = 62,
    Value = 42,
    {T, P} = append(
        [{Timestamp, Value}],
        T0,
        Points,
        Interval,
        Persisted
    ),
    ?assertEqual(50, T),
    ?assertEqual([[1], [42]], array:to_list(P)).

append_skip_test() ->
    T0 = 50,
    Points = array:from_list([[1]], null),
    Interval = 10,
    Persisted = [],
    Timestamp = 72,
    Value = 42,
    {T, P} = append(
        [{Timestamp, Value}],
        T0,
        Points,
        Interval,
        Persisted
    ),
    ?assertEqual(50, T),
    ?assertEqual([[1], null, [42]], array:to_list(P)).

append_negative_accept_test() ->
    T0 = 50,
    Points = array:from_list([[1]]),
    Interval = 10,
    Persisted = [],
    Timestamp = 32,
    Value = 40,
    {T, P} = append(
        [{Timestamp, Value}],
        T0,
        Points,
        Interval,
        Persisted
    ),
    ?assertEqual(30, T),
    ?assertEqual([[40], null, [1]], array:to_list(P)).

append_negative_reject_test() ->
    T0 = 50000,
    Points = array:from_list([[1]]),
    Interval = 10,
    Persisted = [{40000, 12}],
    Timestamp = 30000,
    Value = 40,
    {T, P} = append(
        [{Timestamp, Value}],
        T0,
        Points,
        Interval,
        Persisted
    ),
    ?assertEqual(T0, T),
    ?assertEqual(Points, P).

prop_append() ->
    ?FORALL(
        {T0, Points0, Interval, Persisted, ToAppend},
        {
            timestamp(),
            [[number()]],
            interval(),
            persisted(),
            [{timestamp(), number()}]
        },
        begin
            {T1, Array} = append(
                ToAppend,
                T0,
                array:from_list(Points0, null),
                Interval,
                Persisted
            ),
            Points1 = array:to_list(Array),
            true
        end
    ).

append_point_test() ->
    DP0 = array:new({default, null}),
    DP1 = append_point(0, 45, DP0),
    ?assertEqual([[45]], array:to_list(DP1)),
    DP2 = append_point(0, 50, DP1),
    ?assertEqual([[50, 45]], array:to_list(DP2)),
    DP3 = append_point(2, 42, DP2),
    ?assertEqual([[50, 45], null, [42]], array:to_list(DP3)).

prop_append_point() ->
    ?FORALL(
        {Idx, Value, List},
        {pos_integer(), number(), [[number()]]},
        begin
            Array1 = append_point(Idx, Value, array:from_list(List, null)),
            true = Value == hd(lists:nth(Idx + 1, array:to_list(Array1)))
        end
    ).

read_int_null_test() ->
    Result = read_int(
        100,
        200,
        20,
        fun oscilloscope_aggregations:avg/1,
        undefined,
        array:new({default, null})
    ),
    ?assertEqual({100, 200, lists:duplicate(6, null)}, Result).

read_int_some_end_test() ->
    Points = [[20.0] || _ <- lists:seq(1, 10)],
    Result = read_int(
        100,
        200,
        20,
        fun oscilloscope_aggregations:avg/1,
        140,
        array:from_list(Points, null)
    ),
    Expected = {100, 200, [null, null, 20.0, 20.0, 20.0, 20.0]},
    ?assertEqual(Expected, Result).

read_int_some_start_test() ->
    Points = [[20.0] || _ <- lists:seq(1, 3)],
    Result = read_int(
        100,
        200,
        20,
        fun oscilloscope_aggregations:avg/1,
        100,
        array:from_list(Points, null)
    ),
    Expected = {100, 200, [20.0, 20.0, 20.0, null, null, null]},
    ?assertEqual(Expected, Result).

read_int_middle_test() ->
    Points = [[20.0], [20.0]],
    Result = read_int(
        12330,
        12360,
        10,
        fun oscilloscope_aggregations:avg/1,
        12340,
        array:from_list(Points, null)
    ),
    Expected = {12330, 12360, [null, 20.0, 20.0, null]},
    ?assertEqual(Expected, Result).

read_int_all_test() ->
    Points = [[20.0] || _ <- lists:seq(1, 10)],
    Result = read_int(
        100,
        200,
        20,
        fun oscilloscope_aggregations:avg/1,
        80,
        array:from_list(Points, null)
    ),
    Expected = {100, 200, [20.0, 20.0, 20.0, 20.0, 20.0, 20.0]},
    ?assertEqual(Expected, Result).

prop_read_int() ->
    ?FORALL(
        {From0, Until0, Interval, T, Points0},
        {timestamp(), timestamp(), interval(), timestamp(), [[float()]]},
        begin
            case From0 =< Until0 of
                true ->
                    {From1, Until1, Points1} = read_int(
                        From0,
                        Until0,
                        Interval,
                        fun oscilloscope_aggregations:avg/1,
                        T,
                        array:from_list(Points0, null)
                    ),
                    %% Number of points matches the provided range
                    true = ((Until1 - From1) / Interval) + 1 == length(Points1);
                false ->
                    true
            end
        end
    ).

calculate_query_bounds_test() ->
    ?assertEqual({10, 20}, calculate_query_bounds(12, 17, 10)),
    ?assertEqual({10, 20}, calculate_query_bounds(10, 20, 10)),
    ?assertEqual({10, 20}, calculate_query_bounds(12, 11, 10)).

prop_calculate_query_bounds() ->
    ?FORALL(
        {From0, Until0, Interval},
        {timestamp(), timestamp(), interval()},
        begin
            {From1, Until1} = calculate_query_bounds(From0, Until0, Interval),
            true = From1 rem Interval == 0,
            true = Until1 rem Interval == 0
        end
    ).

maybe_trim_test() ->
    ?assertEqual(
        {100, array:from_list([a, b, c, d], null)},
        maybe_trim(100, array:from_list([a, b, c, d], null), 10, 20)
    ),
    ?assertEqual(
        {120, array:from_list([c, d], null)},
        maybe_trim(100, array:from_list([a, b, c, d], null), 10, 2)
    ).

prop_maybe_trim() ->
    ?FORALL(
        {Time0, Points0, Interval, Count},
        {timestamp(), [any()], interval(), count()},
        begin
            {Time1, Array} = maybe_trim(
                Time0,
                array:from_list(Points0, null),
                Interval,
                Count
            ),
            Points1 = array:to_list(Array),
            %% Time never decreases
            true = Time1 >= Time0,
            %% We remove the same number of points as we accelerate
            true = (Time1 - Time0) / Interval == length(Points0) - length(Points1),
            %% We never leave more than Count points in place
            true = length(Points1) =< Count,
            %% Points are trimmed from the end
            true = lists:suffix(Points1, Points0)
        end
    ).

proper_test_() ->
    {
        timeout,
        1000,
        [] = proper:module(?MODULE, [{to_file, user}, {numtests, 1}])
    }.

-endif.
