-module(apod_eunit).

-include_lib("eunit/include/eunit.hrl").

update_empty_test() ->
    {ok, W} = apod:new(rectangular, avg, 10, 8640, -1),
    ok = apod:update(W, 12345, 42.0),
    ?assertEqual(12340, apod:earliest_time(W)),
    ?assertEqual([42.0], apod:to_list(W)).

update_adjacent_test() ->
    {ok, W} = apod:new(rectangular, avg, 10, 8640, -1),
    ok = apod:update(W, 50, 1.0),
    ok = apod:update(W, 62, 2.0),
    ?assertEqual(50, apod:earliest_time(W)),
    ?assertEqual([1.0, 2.0], apod:to_list(W)).

update_skip_test() ->
    {ok, W} = apod:new(rectangular, avg, 10, 8640, -1),
    ok = apod:update(W, 50, 1.0),
    ok = apod:update(W, 72, 2.0),
    ?assertEqual(50, apod:earliest_time(W)),
    ?assertEqual([1.0, undefined, 2.0], apod:to_list(W)).

update_negative_accept_test() ->
    {ok, W} = apod:new(rectangular, avg, 10, 8640, -1),
    ok = apod:update(W, 50, 1.0),
    ok = apod:update(W, 30, 2.0),
    ?assertEqual(30, apod:earliest_time(W)),
    ?assertEqual([2.0, undefined, 1.0], apod:to_list(W)).

update_negative_reject_test() ->
    {ok, W} = apod:new(rectangular, avg, 10, 8640, 40000),
    ok = apod:update(W, 50000, 1.0),
    ok = apod:update(W, 30000, 2.0),
    ?assertEqual(50000, apod:earliest_time(W)),
    ?assertEqual([1.0], apod:to_list(W)).

automatic_truncate_test() ->
    {ok, W} = apod:new(rectangular, avg, 10, 2, -1),
    ok = apod:update(W, 100, 1.0),
    ok = apod:update(W, 110, 2.0),
    ?assertEqual(100, apod:earliest_time(W)),
    ?assertEqual([1.0, 2.0], apod:to_list(W)),
    ok = apod:update(W, 120, 3.0),
    ?assertEqual(110, apod:earliest_time(W)),
    ?assertEqual([2.0, 3.0], apod:to_list(W)),
    ok = apod:update(W, 200, 4.0),
    ?assertEqual(200, apod:earliest_time(W)),
    ?assertEqual([4.0], apod:to_list(W)).

manual_truncate_test() ->
    {ok, W} = apod:new(rectangular, avg, 10, 8640, -1),
    ok = apod:update(W, 100, 1.0),
    ok = apod:update(W, 110, 2.0),
    ok = apod:update(W, 120, 3.0),
    ok = apod:update(W, 130, 4.0),
    ok = apod:truncate(W, 100),
    ?assertEqual(110, apod:earliest_time(W)),
    ?assertEqual([2.0, 3.0, 4.0], apod:to_list(W)),
    ok = apod:update(W, 150, 5.0),
    ok = apod:truncate(W, 140),
    ?assertEqual(150, apod:earliest_time(W)),
    ?assertEqual([5.0], apod:to_list(W)),
    ok = apod:truncate(W, 160),
    ?assertEqual(undefined, apod:earliest_time(W)),
    ?assertEqual(undefined, apod:to_list(W)).

chunkify_test() ->
    {ok, W} = apod:new(rectangular, avg, 10, 8640, -1),
    ok = apod:update(W, 0, 0.0),
    ok = apod:update(W, 10, 1.0),
    ok = apod:update(W, 20, 2.0),
    Chunked0 = apod:chunkify(W, 0, 1000000, 1000000),
    ?assertEqual(
        [],
        Chunked0
    ),
    Chunked1 = apod:chunkify(W, 0, 12, 1000000),
    Decoded1 = lists:map(
        fun({I, V, C}) -> {I, apod:inflate(V), C} end,
        Chunked1
    ),
    ?assertEqual([{0, [0.0, 1.0, 2.0], 3}], Decoded1),
    lists:foreach(
        fun(I) -> apod:update(W, 10 * I, float(I)) end,
        lists:seq(0, 21)
    ),
    Chunked2 = apod:chunkify(W, 0, 20, 40),
    Decoded2 = lists:map(
        fun({I, V, C}) -> {I, apod:inflate(V), C} end,
        Chunked2
    ),
    ?assertEqual([
        {0, [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0], 11},
        {110, [11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0], 11}
    ], Decoded2).

read_test() ->
    {ok, W} = apod:new(rectangular, avg, 10, 8640, -1),
    ok = apod:update(W, 0, 0.0),
    ok = apod:update(W, 10, 1.0),
    ok = apod:update(W, 20, 2.0),
    ok = apod:update(W, 30, 3.0),
    ok = apod:update(W, 40, 4.0),
    ?assertEqual(
        {0, 30, [0.0, 1.0, 2.0, 3.0]},
        apod:read(W, 0, 30)
    ),
    ?assertEqual(
        {40, 40, [4.0]},
        apod:read(W, 40, 200)
    ),
    ok = apod:update(W, 60, 6.0),
    ?assertEqual(
        {40, 60, [4.0, undefined, 6.0]},
        apod:read(W, 40, 60)
    ).
