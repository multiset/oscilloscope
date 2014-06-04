-module(oscilloscope_persistence_eunit).

-include_lib("oscilloscope_persistence/include/oscilloscope_persistence.hrl").
-include_lib("eunit/include/eunit.hrl").

calculate_query_bounds_test() ->
    ?assertEqual(
        not_found,
        oscilloscope_persistence:calculate_query_bounds(
            100, 200, 10, []
        )
    ),
    ?assertEqual(
        {100, 100},
        oscilloscope_persistence:calculate_query_bounds(
            100, 200, 10, [{100, 11}, {110, 5}]
        )
    ).

chunkify_test() ->
    %% No chunking
    Input = [{0, 1.0}, {10, 2.0}, {20, 3.0}],
    ?assertEqual(
        [],
        oscilloscope_persistence:chunkify(
            Input,
            1000000,
            1000000
        )
    ),
    %% Chunking each value
    Chunked = oscilloscope_persistence:chunkify(
        Input,
        11,
        1000000
    ),
    Decoded = lists:map(fun({I, V, _}) -> {I, ?VALDECODE(V)} end, Chunked),
    ?assertEqual([{0, [1.0, 2.0, 3.0]}], Decoded),
    %% Chunking multiple values together
    Input1 = [{I * 10, float(I)} || I <- lists:seq(0, 21)],
    Chunked1 = oscilloscope_persistence:chunkify(
        Input1,
        30,
        65
    ),
    Decoded1 = lists:map(fun({I, V, _}) -> {I, ?VALDECODE(V)} end, Chunked1),
    ?assertEqual([
        {0, [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0]},
        {120, [12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0]}
    ], Decoded1).
