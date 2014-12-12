-module(osc_meta_eunit).

-include_lib("eunit/include/eunit.hrl").

match_props_test() ->
    %% Strict matches match
    ?assertEqual(true, osc_meta_window_configuration:match_props(
        [{<<"foo">>, <<"bar">>}],
        [{<<"foo">>, <<"bar">>}]
    )),
    %% Strict mismatches don't match
    ?assertEqual(false, osc_meta_window_configuration:match_props(
        [{<<"foo">>, <<"bar">>}],
        [{<<"foo">>, <<"baz">>}]
    )),
    %% Basic regular expressions match
    ?assertEqual(true, osc_meta_window_configuration:match_props(
        [{<<"foo">>, <<"ba.{1}">>}],
        [{<<"foo">>, <<"baz">>}]
    )),
    %% Strict matches don't match if they don't have all props
    ?assertEqual(false, osc_meta_window_configuration:match_props(
        [{<<"foo">>, <<"bar">>}, {<<"baz">>, <<"qux">>}],
        [{<<"foo">>, <<"baz">>}]
    )),
    %% Not all properties are required to match
    ?assertEqual(true,  osc_meta_window_configuration:match_props(
        [{<<"foo">>, <<"bar">>}],
        [{<<"foo">>, <<"bar">>}, {<<"baz">>, <<"qux">>}]
    )).

