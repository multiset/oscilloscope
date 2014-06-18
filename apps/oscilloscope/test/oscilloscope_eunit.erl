-module(oscilloscope_eunit).

-include_lib("eunit/include/eunit.hrl").

divide_array_test() ->
    ?assertEqual({[], []}, oscilloscope_cache:divide_array(array:new(), 4)),
    ?assertEqual(
        {[1, 2], [3, 4]},
        oscilloscope_cache:divide_array(array:from_list([1, 2, 3, 4]), 2)
    ),
    ?assertEqual(
        {[1, 2, 3, 4], []},
        oscilloscope_cache:divide_array(array:from_list([1, 2, 3, 4]), 7)
    ).

append_null_test() ->
    T0 = undefined,
    Points = array:new({default, null}),
    Interval = 10,
    Persisted = [],
    Timestamp = 12345,
    Value = 42,
    {T, P} = oscilloscope_cache:append(
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
    {T, P} = oscilloscope_cache:append(
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
    {T, P} = oscilloscope_cache:append(
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
    {T, P} = oscilloscope_cache:append(
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
    {T, P} = oscilloscope_cache:append(
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
    DP1 = oscilloscope_cache:append_point(0, 45, DP0),
    ?assertEqual([[45]], array:to_list(DP1)),
    DP2 = oscilloscope_cache:append_point(0, 50, DP1),
    ?assertEqual([[50, 45]], array:to_list(DP2)),
    DP3 = oscilloscope_cache:append_point(2, 42, DP2),
    ?assertEqual([[50, 45], null, [42]], array:to_list(DP3)).


%% read_int_null_test() ->
%%     Result = oscilloscope_cache:read_int(
%%         100,
%%         200,
%%         20,
%%         fun oscilloscope_aggregations:avg/1,
%%         undefined,
%%         array:new({default, null})
%%     ),
%%     ?assertEqual({100, 200, lists:duplicate(6, null)}, Result).

%% read_int_some_end_test() ->
%%     Points = [[20.0] || _ <- lists:seq(1, 10)],
%%     Result = oscilloscope_cache:read_int(
%%         100,
%%         200,
%%         20,
%%         fun oscilloscope_aggregations:avg/1,
%%         140,
%%         array:from_list(Points, null)
%%     ),
%%     Expected = {100, 200, [null, null, 20.0, 20.0, 20.0, 20.0]},
%%     ?assertEqual(Expected, Result).

%% read_int_some_start_test() ->
%%     Points = [[20.0] || _ <- lists:seq(1, 3)],
%%     Result = oscilloscope_cache:read_int(
%%         100,
%%         200,
%%         20,
%%         fun oscilloscope_aggregations:avg/1,
%%         100,
%%         array:from_list(Points, null)
%%     ),
%%     Expected = {100, 200, [20.0, 20.0, 20.0, null, null, null]},
%%     ?assertEqual(Expected, Result).

%% read_int_middle_test() ->
%%     Points = [[20.0], [20.0]],
%%     Result = oscilloscope_cache:read_int(
%%         12330,
%%         12360,
%%         10,
%%         fun oscilloscope_aggregations:avg/1,
%%         12340,
%%         array:from_list(Points, null)
%%     ),
%%     Expected = {12330, 12360, [null, 20.0, 20.0, null]},
%%     ?assertEqual(Expected, Result).

%% read_int_all_test() ->
%%     Points = [[20.0] || _ <- lists:seq(1, 10)],
%%     Result = oscilloscope_cache:read_int(
%%         100,
%%         200,
%%         20,
%%         fun oscilloscope_aggregations:avg/1,
%%         80,
%%         array:from_list(Points, null)
%%     ),
%%     Expected = {100, 200, [20.0, 20.0, 20.0, 20.0, 20.0, 20.0]},
%%     ?assertEqual(Expected, Result).

adjust_query_range_test() ->
    ?assertEqual({10, 20}, oscilloscope_util:adjust_query_range(12, 17, 10)),
    ?assertEqual({10, 20}, oscilloscope_util:adjust_query_range(10, 20, 10)),
    ?assertEqual({10, 20}, oscilloscope_util:adjust_query_range(12, 11, 10)).

maybe_trim_test() ->
    ?assertEqual(
        {100, array:from_list([a, b, c, d], null)},
        oscilloscope_cache:maybe_trim(100, array:from_list([a, b, c, d], null), 10, 20)
    ),
    ?assertEqual(
        {120, array:from_list([c, d], null)},
        oscilloscope_cache:maybe_trim(100, array:from_list([a, b, c, d], null), 10, 2)
    ).