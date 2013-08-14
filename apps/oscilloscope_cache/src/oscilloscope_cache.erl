-module(oscilloscope_cache).

-export([
    start/0,
    stop/0,
    process/3,
    read/3
]).

-export([
    start_link/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(st, {
    metric,
    resolution,
    aggregation_fun,
    bytes_per_point,
    last_persist
}).

%% Minimim amount of seconds to wait before persisting to disk
-define(MIN_PERSIST_AGE, 300).

start() ->
    application:start(oscilloscope_cache).

stop() ->
    ok.

process(Metric, Timestamp, Value) ->
    multicast(Metric, {process, Timestamp, Value}).

read(Metric, From, Until) ->
    {Megaseconds, Seconds, _} = erlang:now(),
    QueryTime = Megaseconds * 1000000 + Seconds,
    Resolutions = multicall(Metric, get_resolution),
    Resolutions1 = lists:sort(
        fun({_, {ok, {IntervalA, _}}}, {_, {ok, {IntervalB, _}}}) ->
            IntervalA >= IntervalB
        end,
        Resolutions
    ),
    {Pid, _Resolution} = lists:foldl(
        fun({_, {ok, {Interval, Count}}}=R, Result) ->
            case QueryTime - Interval * Count < From of
                true -> R;
                false -> Result
            end
        end, hd(Resolutions1), Resolutions1),
    gen_server:call(Pid, {read, From, Until}).

start_link(Metric, Resolution) ->
    gen_server:start_link(?MODULE, {Metric, Resolution}, []).

init({Metric, Resolution}) ->
    ok = pg2:join(Metric, self()),
    %% TODO: get these from persistent store
    AggregationFun = fun oscilloscope_cache_aggregations:avg/1,
    BytesPerPoint = 1,
    LastPersist = 0,
    case read({Metric, Resolution}) of
        not_found -> write({Metric, Resolution}, gb_trees:empty());
        _ -> ok
    end,
    {ok, #st{
        metric = Metric,
        resolution = Resolution,
        aggregation_fun = AggregationFun,
        bytes_per_point = BytesPerPoint,
        last_persist = LastPersist
    }}.

handle_call(get_resolution, _From, #st{resolution=R}=State) ->
    {reply, {ok, R}, State};
handle_call({read, From, Until}, _From, State) ->
    #st{metric=M, aggregation_fun=AF, resolution=R} = State,
    Points = read({M, R}),
    InRange = lists:foldl(
        fun({T, Vs}, Acc) ->
            case T >= From andalso T =< Until of
                true -> [{T, AF(Vs)}|Acc];
                false -> Acc
            end
        end, [], gb_trees:to_list(Points)
    ),
    {reply, {ok, lists:reverse(InRange)}, State};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, error, State}.

handle_cast({process, Timestamp, Value}, State) ->
    #st{metric=M, resolution={I, C}=R, last_persist=LP} = State,
    Timestamp1 = Timestamp - (Timestamp rem I),
    %% We claim a ?MIN_PERSIST_AGE maximum age
    %% but that's enforced by not persisting newer points.
    %% In practice we'll accept anything that's newer than the last persist.
    if
        Timestamp1 > LP ->
            OldPoints = read({M, R}),
            NewPoints = case gb_trees:lookup(Timestamp1, OldPoints) of
                none ->
                    gb_trees:enter(Timestamp1, [Value], OldPoints);
                {value, Vs} ->
                    gb_trees:enter(Timestamp1, [Value|Vs], OldPoints)
            end,
            write({M, R}, NewPoints);
        true ->
            ok
    end,
    {noreply, State, 0};
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(timeout, State) ->
    {noreply, maybe_persist_points(State)};
handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

maybe_persist_points(State) ->
    % TODO
    State.

multicast(Metric, Msg) ->
    Pids = get_pids(Metric),
    lists:map(fun(P) -> gen_server:cast(P, Msg) end, Pids).

multicall(Metric, Msg) ->
    Pids = get_pids(Metric),
    lists:map(fun(P) -> {P, gen_server:call(P, Msg)} end, Pids).

get_pids(Metric) ->
    case pg2:get_members(Metric) of
        {error, {no_such_group, Metric}} ->
            {ok, _Pid} = oscilloscope_cache_sup:spawn_cache(Metric),
            pg2:get_members(Metric);
        P ->
            P
    end.

read(Key) ->
    case erp:q(["GET", term_to_binary(Key)]) of
        {ok, undefined} -> not_found;
        {ok, Value} -> binary_to_term(Value)
    end.

write(Key, Value) ->
    {ok, <<"OK">>} = erp:q(["SET", term_to_binary(Key), term_to_binary(Value)]),
    ok.
