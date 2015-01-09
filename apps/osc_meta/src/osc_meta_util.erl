-module(osc_meta_util).

-export([
    encode_props/1,
    decode_props/1,
    parse_window_type/1,
    parse_window_aggregation/1
]).

encode_props(Props) ->
    term_to_binary(lists:sort(Props)).

decode_props(Binary) ->
    binary_to_term(Binary).

parse_window_type(<<"rectangular">>) -> rectangular.

parse_window_aggregation(<<"avg">>) -> avg.
