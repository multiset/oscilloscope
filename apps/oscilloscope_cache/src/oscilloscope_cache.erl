-module(oscilloscope_cache).

-export([
    start/0,
    stop/0,
    process/3,
    read/3
]).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(st, {
    metric,
    interval,
    aggregation_fun,
    bytes_per_point,
    last_persist
}).

%% Minimim amount of seconds to wait before persisting to disk
-define(MIN_PERSIST_AGE, 300).

start() ->
    application:start(gproc),
    application:start(oscilloscope_cache).

stop() ->
    ok.

process(Metric, Timestamp, Value) ->
    %% TODO: multiple retentions
    gen_server:cast(maybe_spawn_cache(Metric), {process, Timestamp, Value}).

read(Metric, From, Until) ->
    %% TODO: multiple retentions
    gen_server:call(maybe_spawn_cache(Metric), {read, From, Until}).

start_link(Metric) ->
    gen_server:start_link(?MODULE, Metric, []).

init(Metric) ->
    gproc:reg({n, l, Metric}, ignored),
    %% TODO: get these from persistent store
    Interval = 10,
    AggregationFun = fun oscilloscope_cache_aggregations:avg/1,
    BytesPerPoint = 1,
    LastPersist = 0,
    case read(Metric) of
        not_found -> write(Metric, gb_trees:empty());
        _ -> ok
    end,
    {ok, #st{
        metric = Metric,
        interval = Interval,
        aggregation_fun = AggregationFun,
        bytes_per_point = BytesPerPoint,
        last_persist = LastPersist
    }}.

handle_call({read, From, Until}, _From, State) ->
    #st{metric=M, aggregation_fun=AF} = State,
    Points = read(M),
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
    #st{metric=M, interval=I, last_persist=LP} = State,
    Timestamp1 = Timestamp - (Timestamp rem I),
    %% We claim a ?MIN_PERSIST_AGE maximum age
    %% but that's enforced by not persisting newer points.
    %% In practice we'll accept anything that's newer than the last persist.
    if Timestamp1 > LP ->
        OldPoints = read(M),
        NewPoints = case gb_trees:lookup(Timestamp1, OldPoints) of
            none ->
                gb_trees:enter(Timestamp1, [Value], OldPoints);
            {value, Vs} ->
                gb_trees:enter(Timestamp1, [Value|Vs], OldPoints)
        end,
        write(M, NewPoints)
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

maybe_spawn_cache(Metric) ->
    case gproc:where({n, l, Metric}) of
        undefined ->
            {ok, Pid} = oscilloscope_cache_sup:spawn_cache(Metric),
            Pid;
        Pid -> Pid
    end.

read(Metric) ->
    case erp:q(["GET", Metric]) of
        {ok, undefined} -> not_found;
        {ok, Value} -> binary_to_term(Value)
    end.

write(Metric, Value) ->
    {ok, <<"OK">>} = erp:q(["SET", Metric, term_to_binary(Value)]),
    ok.
