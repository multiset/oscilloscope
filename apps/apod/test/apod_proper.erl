-module(apod_proper).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("osc/include/osc_types.hrl").

-type op() :: {update, timestamp()}
    | {read, timestamp(), timestamp()}
    | {truncate, timestamp()}.

debug() ->
    case get(count) of
        undefined ->
            put(count, 0);
        C when C rem 1000 =:= 0 ->
            ?debugFmt("Count: ~p", [C]),
            put(count, C + 1);
        C ->
            put(count, C + 1)
    end.

debug(Msg) ->
    ?debugFmt("~p", [Msg]),
    debug().

apod_op(W, Interval, {update, Timestamp}) ->
    ok = apod:update(W, Timestamp, float(Timestamp));
apod_op(W, Interval, {read, From0, Until0}) ->
    case apod:read(W, From0, Until0) of
        undefined ->
            %% This is difficult to test, maybe
            ok;
        error ->
            %% Also difficult to test?
            ok;
        {From1, Until1, Points} ->
            true = Until1 >= From1,
            true = (Until1 - From1) rem Interval == 0,
            true = length(Points) == ((Until1 - From1) div Interval) + 1
    end;
apod_op(W, Interval, {truncate, Time}) ->
    ok = apod:truncate(W, Time),
    true = apod:earliest_time(W) >= Time + Interval - (Time rem Interval).

prop_apod_rect_avg() ->
    ?FORALL(
        Ops,
        non_empty(list(op())),
        begin
            {ok, W} = apod:new(rectangular, avg, 10, 8640, -1),
            lists:map(
                fun(Op) ->
                    apod_op(W, 10, Op)
                end,
                Ops
            ),
            true
        end
    ).

proper_test_() ->
    {
        timeout,
        100000,
        [] = proper:module(?MODULE, [{to_file, user}, {numtests, 1000}])
    }.
