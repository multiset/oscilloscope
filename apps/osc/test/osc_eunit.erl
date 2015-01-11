-module(osc_eunit).

-include_lib("eunit/include/eunit.hrl").

adjust_query_range_test() ->
    ?assertEqual({10, 20}, osc_util:adjust_query_range(12, 17, 10)),
    ?assertEqual({10, 20}, osc_util:adjust_query_range(10, 20, 10)),
    ?assertEqual({10, 20}, osc_util:adjust_query_range(12, 11, 10)).
