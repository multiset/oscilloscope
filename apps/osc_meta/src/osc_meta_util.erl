-module(osc_meta_util).

-export([
    parse_window_type/1,
    parse_window_aggregation/1
]).

parse_window_type(<<"rectangular">>) -> rectangular.

parse_window_aggregation(<<"avg">>) -> avg.
