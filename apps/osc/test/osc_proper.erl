-module(osc_proper).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("osc/include/osc_types.hrl").

-type read_kernel() :: {integer(), pos_integer()}.

init_reads(From, Interval, Kernels) ->
    lists:sort(
        fun({A, _, _}, {B, _, _}) -> A < B end,
        lists:map(fun(K) -> init_read(From, Interval, K) end, Kernels)
    ).

init_read(From, Interval, {Offset, Count}) ->
    RFrom0 = From + Offset,
    RFrom = RFrom0 - (RFrom0 rem Interval),
    RUntil = RFrom + Interval * (Count - 1),
    Values = lists:duplicate(Count, 0.0),
    {RFrom, RUntil, Values}.

prop_merge_reads() ->
    ?FORALL(
        {From0, Count, Interval, Cache, Kernels},
        {timestamp(), pos_integer(), interval(), boolean(), list(read_kernel())},
        begin
            {From1, Until1} = osc_util:adjust_query_range(
                From0,
                From0 + Count * Interval,
                Interval
            ),
            Reads0 = init_reads(From1, Interval, Kernels),
            Reads1 = case Cache of
                true ->
                    [no_data|Reads0];
                false ->
                    Reads0
            end,

            Merged = osc:merge_reads(From1, Until1, Interval, Reads1),
            true = length(Merged) == (((Until1 - From1) div Interval) + 1)
        end
    ).

proper_test_() ->
    {
        timeout,
        100000,
        [] = proper:module(?MODULE, [{to_file, user}, {numtests, 1000}])
    }.
