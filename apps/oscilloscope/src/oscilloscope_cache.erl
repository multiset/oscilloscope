-module(oscilloscope_cache).

-export([
    new/1,
    refresh/1,
    update/2,
    read/3
]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

-record(cache, {
    aggregation,
    resolutions
}).

-record(resolution, {
    meta,
    t,
    points
}).

new(Meta) ->
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
    #cache{aggregation=AggregationFun, resolutions=Resolutions}.

refresh(Cache) ->
    Cache.

update(Points, #cache{resolutions=Resolutions0}=Cache) ->
    Resolutions1 = lists:map(
        fun(Resolution) -> update_int(Points, Resolution) end,
        Resolutions0
    ),
    Cache#cache{resolutions=Resolutions1}.

read(From, Until, #cache{resolutions=Resolutions, aggregation=Aggregation}) ->
    #resolution{meta=Meta, t=T, points=Points}=Resolution = select_resolution(
        From,
        Resolutions
    ),
    Interval = oscilloscope_metadata_resolution:interval(Meta),
    Points = read_int(From, Until, Interval, Aggregation, T, Points),
    {From, Until, Resolution, Points}.

update_int(Points, #resolution{meta=Meta, t=T0, points=Points0}=Resolution) ->
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
            case oldest_timestamp(Resolution) =< From of
                true -> Resolution;
                false -> Selected
            end
        end,
        R,
        Rs
    ).

oldest_timestamp(#resolution{meta=Meta, t=T}) ->
    case oscilloscope_metadata_resolution:persisted(Meta) of
        [] ->
            T;
        [{Timestamp, _Count}|_] ->
            Timestamp
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
            Result ++ lists:duplicate(Missing, null);
        false ->
            PointCount = ((Until - From) div Interval),
            lists:duplicate(PointCount, null)
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

append_point_test() ->
    DP0 = array:new({default, null}),
    DP1 = append_point(0, 45, DP0),
    ?assertEqual([[45]], array:to_list(DP1)),
    DP2 = append_point(0, 50, DP1),
    ?assertEqual([[50, 45]], array:to_list(DP2)),
    DP3 = append_point(2, 42, DP2),
    ?assertEqual([[50, 45], null, [42]], array:to_list(DP3)).

read_int_null_test() ->
    Result = read_int(
        100,
        200,
        20,
        fun oscilloscope_aggregations:avg/1,
        undefined,
        array:new({default, null})
    ),
    ?assertEqual(lists:duplicate(5, null), Result).

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
    Expected = [null, null, 20.0, 20.0, 20.0, 20.0],
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
    Expected = [20.0, 20.0, 20.0, null, null, null],
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
    Expected = [null, 20.0, 20.0, null],
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
    Expected = [20.0, 20.0, 20.0, 20.0, 20.0, 20.0],
    ?assertEqual(Expected, Result).

calculate_query_bounds_test() ->
    ?assertEqual({10, 20}, calculate_query_bounds(12, 17, 10)),
    ?assertEqual({10, 20}, calculate_query_bounds(10, 20, 10)),
    ?assertEqual({10, 20}, calculate_query_bounds(12, 11, 10)).

maybe_trim_test() ->
    ?assertEqual(
        {100, array:from_list([a, b, c, d], null)},
        maybe_trim(100, array:from_list([a, b, c, d], null), 10, 20)
    ),
    ?assertEqual(
        {120, array:from_list([c, d], null)},
        maybe_trim(100, array:from_list([a, b, c, d], null), 10, 2)
    ).

-endif.
