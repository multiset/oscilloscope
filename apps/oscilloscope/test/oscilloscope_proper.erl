-module(oscilloscope_proper).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

prop_append() ->
    ?FORALL(
        {TKernel, Points0, Interval, {PKTime, PKCounts}, ToAppend},
        {
            timestamp(),
            [[number()]],
            interval(),
            {pos_integer(), [pos_integer()]},
            [{timestamp(), number()}]
        },
        begin
            %% Initialize types based on random values
            Persisted = lists:reverse(lists:foldl(
                fun(Count, [{T, C}|_]=Acc) ->
                    [{T + C * Interval, Count}|Acc]
                end,
                [{PKTime - (PKTime rem Interval), hd(PKCounts)}],
                tl(PKCounts)
            )),
            LastPersist = oscilloscope_cache:last_persisted_time(
                Persisted,
                Interval
            ),
            T0 = (LastPersist + Interval) + (TKernel - (TKernel rem Interval)),
            {T1, Array} = oscilloscope_cache:append(
                ToAppend,
                T0,
                array:from_list(Points0, null),
                Interval,
                Persisted
            ),
            Points1 = array:to_list(Array),
            [{EarliestT, _}|_] = lists:sort(ToAppend),
            T = EarliestT - (EarliestT rem Interval),
            %% T1 is set to the smallest sensible value
            true = T1 == case {T =< LastPersist, T =< T0} of
                {true, _} ->
                    T0;
                {false, true} ->
                    T;
                _ ->
                    T0
            end,
            %% Total points in the cache should equal the number of valid points
            %% (i.e., T > LastPersistTime) in ToAppend plus the original number
            %% of points, minus any null filler values. Note that point lists
            %% must be flattened since each array is a list of lists and we
            %% allow multiple entries to the same time range
            PointsActuallyAdded = lists:filter(
                fun({Time, _Value}) -> Time > LastPersist end,
                ToAppend
            ),
            TotalPoints0 = length(lists:flatten(Points0)) -
                length(lists:filter(fun(I) -> I == null end, Points0)),
            TotalPoints1 = length(lists:flatten(Points1)) -
                length(lists:filter(fun(I) -> I == null end, Points1)),
            true = TotalPoints1 == TotalPoints0 + length(PointsActuallyAdded)
         end
    ).


prop_append_point() ->
    ?FORALL(
        {Idx, Value, List},
        {pos_integer(), number(), [value()]},
        begin
            Array1 = oscilloscope_cache:append_point(
                Idx,
                Value,
                array:from_list(List, null)
            ),
            true = Value == hd(lists:nth(Idx + 1, array:to_list(Array1)))
        end
    ).

disabled_prop_read_int() ->
    ?FORALL(
        {From0, Delta, Interval, T, Points0},
        {timestamp(), timestamp(), interval(), timestamp(), [value()]},
        begin
            Until0 = From0 + Delta,
            {From1, Until1, Points1} = oscilloscope_cache:read_int(
                From0,
                Until0,
                Interval,
                fun oscilloscope_aggregations:avg/1,
                T,
                array:from_list(Points0, null)
            ),
            %% Number of points matches the provided range
            true = ((Until1 - From1) / Interval) + 1 == length(Points1)
        end
    ).


prop_adjust_query_range() ->
    ?FORALL(
        {From0, Until0, Interval},
        {timestamp(), timestamp(), interval()},
        begin
            {From1, Until1} = oscilloscope_util:adjust_query_range(
                From0,
                Until0,
                Interval
            ),
            true = From1 rem Interval == 0,
            true = Until1 rem Interval == 0
        end
    ).



prop_maybe_trim() ->
    ?FORALL(
        {Time0, Points0, Interval, Count},
        {timestamp(), [any()], interval(), count()},
        begin
            {Time1, Array} = oscilloscope_cache:maybe_trim(
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
        100000,
        [] = proper:module(?MODULE, [{to_file, user}, {numtests, 1000}])
    }.
