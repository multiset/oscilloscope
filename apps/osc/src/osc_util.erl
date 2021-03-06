-module(osc_util).
-export([
    now/0,
    parse_resolution/1,
    adjust_query_range/3,
    binary_join/2,
    ejsonify/1,
    unejsonify/1
]).

now() ->
    {Megas, Seconds, _Micros} = erlang:now(),
    Megas * 1000000 + Seconds.

parse_resolution(ResolutionString) ->
    Retentions = re:split(ResolutionString, ","),
    parse_retentions(Retentions, []).

parse_retentions([], Acc) ->
    lists:reverse(Acc);
parse_retentions([R|Rs], Acc0) ->
    case re:split(R, ":") of
        [R] -> {error, malformatted_retention, R};
        [Interval, Duration] ->
            IntervalSecs = parse_time_window(Interval),
            DurationSecs = parse_time_window(Duration),
            case {parse_time_window(Interval), parse_time_window(Duration)} of
                {I, D} when is_integer(I) and is_integer(D) ->
                    Acc1 = [{IntervalSecs, DurationSecs div IntervalSecs}|Acc0],
                    parse_retentions(Rs, Acc1);
                {I, D} when is_integer(I) ->
                    D;
                {I, _D} ->
                    I
            end
    end.

parse_time_window(WindowString) ->
     Match = re:run(
        WindowString,
        "^([0-9]+)([s,m,h,d,y])$",
        [{capture, all_but_first, list}]
    ),
    case Match of
        nomatch ->
            {error, malformatted_window, WindowString};
        {match, [Numeral, Unit]} ->
            Multiplier = translate_unit(Unit),
            Multiplier * list_to_integer(Numeral)
    end.

translate_unit("s") -> 1;
translate_unit("m") -> 60;
translate_unit("h") -> 3600;
translate_unit("d") -> 86400;
translate_unit("y") -> 31536000.

adjust_query_range(From0, Until0, Interval) ->
    %% Floor the query's From to the preceding interval bound
    From = From0 - (From0 rem Interval),
    %% Ceil the query's Until to the next interval bound
    Until = case Until0 rem Interval of
        0 -> Until0;
        N -> Until0 + Interval - N
    end,
    {From, Until}.

binary_join([], _Sep) ->
    <<>>;
binary_join([H|T], Sep) ->
    binary_join(T, Sep, H).

binary_join([], _Sep, Acc) ->
    Acc;
binary_join([H|T], Sep, Acc) ->
    binary_join(T, Sep, <<Acc/binary, Sep/binary, H/binary>>).

ejsonify([{_, _}|_]=Proplist) ->
    {[{ejsonify(K), ejsonify(V)} || {K, V} <- Proplist]};
ejsonify(Is) when is_list(Is) ->
    [ejsonify(I) || I <- Is];
ejsonify(I) ->
    I.

unejsonify({[{_, _}|_]=Proplist}) ->
    [{unejsonify(K), unejsonify(V)} || {K, V} <- Proplist];
unejsonify(Is) when is_list(Is) ->
    [unejsonify(I) || I <- Is];
unejsonify(I) ->
    I.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_resolution_success_test() ->
    ?assertEqual(
        [{10, 10}],
        parse_resolution(<<"10s:100s">>)
    ),
    ?assertEqual(
        [{10, 86400}],
        parse_resolution(<<"10s:10d">>)
    ),
    ?assertEqual(
        [{30, 40}],
        parse_resolution(<<"30s:20m">>)
    ),
    ?assertEqual(
        [{1800, 40}],
        parse_resolution(<<"30m:20h">>)
    ),
    ?assertEqual(
        [{10, 10}, {20, 5}],
        parse_resolution(<<"10s:100s,20s:100s">>)
    ).

parse_resolution_failure_test() ->
    ?assertEqual(
        {error, malformatted_retention, <<"30m20y">>},
        parse_resolution(<<"10s:100s,30m20y">>)
    ),
    ?assertEqual(
        {error, malformatted_window, <<"20wolfs">>},
        parse_resolution(<<"10s:100s,30m:20wolfs">>)
    ).

-endif.
