-module(osc_proper).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("osc/include/osc_types.hrl").

-type kernel() :: {timestamp(), pos_integer(), [pos_integer()]}.

initialize({TimeKernel, PersistKernel, PersistCounts}, Interval) ->
    Persisted = case PersistCounts of
        [] ->
            [];
        [P|PCs] ->
            lists:reverse(lists:foldl(
                fun(Count, [{T, C}|_]=Acc) ->
                    [{T + C * Interval, Count}|Acc]
                end,
                [{PersistKernel - (PersistKernel rem Interval), P}],
                PCs
            ))
    end,
    T0 = TimeKernel - (TimeKernel rem Interval),
    {LatestPersist, T1} = case Persisted of
        [] ->
            {undefined, T0};
        Persisted ->
            {LastPersistTime, LastPersistCount} = lists:last(Persisted),
            LP = LastPersistTime + Interval * (LastPersistCount - 1),
            {LP, LP + Interval + T0}
    end,
    {LatestPersist, T1}.


prop_append() ->
    ?FORALL(
        {Kernel, Interval, Points0, ToAppend},
        {
            kernel(),
            interval(),
            [[number()]],
            [{timestamp(), number()}]
        },
        begin
            {LatestPersist, T0} = initialize(Kernel, Interval),
            {T1, Array} = osc_window:append(
                ToAppend,
                T0,
                array:from_list(Points0, null),
                Interval,
                LatestPersist
            ),
            Points1 = array:to_list(Array),
            [{EarliestT, _}|_] = lists:sort(ToAppend),
            T = EarliestT - (EarliestT rem Interval),
            %% T1 is set to the smallest sensible value
            TooOld = case LatestPersist of
                undefined -> false;
                LatestPersist -> T < (LatestPersist + Interval)
            end,
            true = T1 == case {TooOld, T =< T0} of
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
                fun({Time, _Value}) ->
                    case LatestPersist of
                        undefined -> true;
                        LatestPersist -> Time >= (LatestPersist + Interval)
                    end
                end,
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
            Array1 = osc_window:append_point(
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
            {From1, Until1, Points1} = osc_window:read_int(
                From0,
                Until0,
                Interval,
                fun osc_aggregations:avg/1,
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
            {From1, Until1} = osc_util:adjust_query_range(
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
        {Kernel, Points0, Interval, Count},
        {kernel(), [any()], interval(), count()},
        begin
            {LatestPersist, Time0} = initialize(Kernel, Interval),
            {Time1, Array} = osc_window:maybe_trim(
                Time0,
                array:from_list(Points0, null),
                Interval,
                Count,
                LatestPersist
            ),
            Points1 = array:to_list(Array),
            %% Time never decreases
            true = Time1 >= Time0,
            %% We remove the same number of points as we accelerate
            true = (Time1 - Time0) / Interval == length(Points0) - length(Points1),
            %% We never leave more than Count points in place
            true = length(Points1) =< Count,
            %% Points are trimmed from the beginning
            true = lists:suffix(Points1, Points0),
            %% The window's base time is always ahead of the persisted time
            case LatestPersist of
                undefined -> true;
                LatestPersist -> Time1 >= LatestPersist + Interval
            end
        end
    ).

proper_test_() ->
    {
        timeout,
        100000,
        [] = proper:module(?MODULE, [{to_file, user}, {numtests, 1000}])
    }.
