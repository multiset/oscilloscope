-module(osc_persistence_eunit).

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
