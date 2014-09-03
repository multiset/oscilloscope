-module(osc_persistence_proper).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("osc/include/osc_types.hrl").

prop_calculate_query_bounds() ->
    ?FORALL(
        {FromKernel, Delta, Interval, {PKTime, PKCounts}},
        {
            timestamp(),
            pos_integer(),
            interval(),
            {pos_integer(), [pos_integer()]}
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
            From0 = FromKernel - (FromKernel rem Interval),
            Until0 = From0 + Delta * Interval,
            Bounds = osc_persistence:calculate_query_bounds(
                From0,
                Until0,
                Interval,
                Persisted
            ),
            true = case Persisted of
                [] -> Bounds == not_found;
                [{EarliestPersist, _}|Rest] ->
                    case Bounds of
                        not_found -> true;
                        {From1, Until1} ->
                            FromFloored = From1 rem Interval == 0,
                            UntilFloored = Until1 rem Interval == 0,
                            BeforeRange = EarliestPersist =< From1,
                            AfterRange = case Rest of
                                [] -> true;
                                [{T, _}|_] -> T > Until1
                            end,
                            FromFloored
                                andalso UntilFloored
                                andalso BeforeRange
                                andalso AfterRange
                    end
            end
        end
    ).

prop_trim_read() ->
    ?FORALL(
        {FromKernel, UntilDelta, Interval, ReadDelta, Overlap},
        {
            timestamp(),
            pos_integer(),
            interval(),
            pos_integer(),
            pos_integer()
        },
        begin
            From = FromKernel - (FromKernel rem Interval),
            Until = From + UntilDelta * Interval,
            ReadFrom0 = (Until - ReadDelta),
            ReadFrom = erlang:max(0, ReadFrom0 - (ReadFrom0 rem Interval)),
            Read = case From < ReadFrom of
                true ->
                    lists:seq(0, Overlap);
                false ->
                    Count = ((From - ReadFrom) div Interval) + Overlap,
                    lists:seq(0, Count)
            end,
            {StartTime, EndTime, Points} = osc_persistence:trim_read(
                From,
                Until,
                Interval,
                ReadFrom,
                Read
            ),
            true = ((EndTime - StartTime) div Interval) + 1 == length(Points),
            true = EndTime =< Until,
            true = StartTime >= From
        end
    ).


proper_test_() ->
    {
        timeout,
        100000,
        [] = proper:module(?MODULE, [{to_file, user}, {numtests, 1000}])
    }.
