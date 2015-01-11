-module(osc_meta_util).

-export([
    encode_props/1,
    decode_props/1,
    parse_window_type/1,
    parse_window_aggregation/1,
    find_prop_match/2,
    match_props/2
]).

-include_lib("osc/include/osc_types.hrl").
-include_lib("osc_meta/include/osc_meta.hrl").

encode_props(Props) ->
    term_to_binary(lists:sort(Props)).

decode_props(Binary) ->
    binary_to_term(Binary).

parse_window_type(<<"rectangular">>) -> rectangular.

parse_window_aggregation(<<"avg">>) -> avg;
parse_window_aggregation(<<"sum">>) -> sum;
parse_window_aggregation(<<"min">>) -> min;
parse_window_aggregation(<<"max">>) -> max;
parse_window_aggregation(<<"last">>) -> last.

-spec find_prop_match(PropMatchList, MetricProps) -> Match when
    PropMatchList :: [PropMatch],
    PropMatch :: [{PropMatchKey, PropMatchValue}],
    PropMatchKey :: group_tag_key(),
    PropMatchValue :: group_tag_value(),
    MetricProps :: [{prop_key(), prop_value()}],
    Match :: boolean().

find_prop_match([], _MetricProps) ->
    false;
find_prop_match([PropertyList|PLs], MetricProps) ->
    case match_props(PropertyList, MetricProps) of
        true -> true;
        false -> find_prop_match(PLs, MetricProps)
    end.

-spec match_props(PropMatch, MetricProps) -> Match when
    PropMatch :: [{PropMatchKey, PropMatchValue}],
    PropMatchKey :: group_tag_key(),
    PropMatchValue :: group_tag_value(),
    MetricProps :: [{prop_key(), prop_value()}],
    Match :: boolean().

match_props([], _) ->
    true;
match_props([{GroupKey, GroupValue}|KVs], Props) ->
    case proplists:get_value(GroupKey, Props) of
        undefined ->
            false;
        PropValue ->
            Match = re:run(PropValue, GroupValue) =/= nomatch,
            case Match of
                true -> match_props(KVs, Props);
                false -> false
            end
    end.
