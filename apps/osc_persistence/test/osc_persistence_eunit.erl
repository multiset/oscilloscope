-module(osc_persistence_eunit).

-include_lib("osc_persistence/include/osc_persistence.hrl").
-include_lib("eunit/include/eunit.hrl").

calculate_query_bounds_test() ->
    ?assertEqual(
        not_found,
        osc_persistence:calculate_query_bounds(
            100, 200, 10, []
        )
    ),
    ?assertEqual(
        {100, 100},
        osc_persistence:calculate_query_bounds(
            100, 200, 10, [{100, 11}, {210, 5}]
        )
    ).

trim_read_test() ->
    ?assertEqual(
        {150, 200, [1, 2, 3, 4, 5, 6]},
        osc_persistence:trim_read(
            100,
            200,
            10,
            150,
            [1, 2, 3, 4, 5, 6, 7, 8]
        )
    ),
    ?assertEqual(
        {100, 200, [3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]},
        osc_persistence:trim_read(
            100,
            200,
            10,
            80,
            [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
        )
    ),
    ?assertEqual(
        {100, 200, [1, 2, 3, 4, 5, 6]},
        osc_persistence:trim_read(
            100,
            200,
            20,
            100,
            [1, 2, 3, 4, 5, 6]
        )
    ),
    ?assertEqual(
        {100, 200, [1, 2, 3, 4, 5, 6]},
        osc_persistence:trim_read(
            100,
            300,
            20,
            100,
            [1, 2, 3, 4, 5, 6]
        )
    ).


chunkify_test() ->
    %% No chunking
    Input = [{0, 1.0}, {10, 2.0}, {20, 3.0}],
    ?assertEqual(
        [],
        osc_persistence:chunkify(
            Input,
            1000000,
            1000000
        )
    ),
    %% Chunking each value
    Chunked = osc_persistence:chunkify(
        Input,
        11,
        1000000
    ),
    Decoded = lists:map(fun({I, V, _}) -> {I, ?VALDECODE(V)} end, Chunked),
    ?assertEqual([{0, [1.0, 2.0, 3.0]}], Decoded),
    %% Chunking multiple values together
    Input1 = [{I * 10, float(I)} || I <- lists:seq(0, 21)],
    Chunked1 = osc_persistence:chunkify(
        Input1,
        30,
        65
    ),
    Decoded1 = lists:map(fun({I, V, _}) -> {I, ?VALDECODE(V)} end, Chunked1),
    ?assertEqual([
        {0, [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0]},
        {120, [12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0]}
    ], Decoded1).
