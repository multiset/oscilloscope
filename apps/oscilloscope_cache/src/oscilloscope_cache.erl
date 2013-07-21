-module(oscilloscope_cache).

-export([
    start/0,
    stop/0,
    process/1
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
    datapoints
}).

start() ->
    application:start(gproc),
    application:start(oscilloscope_cache).

stop() ->
    ok.

process({Metric, Timestamp, Value}) ->
    Pid = case gproc:where({n, l, Metric}) of
        undefined ->
            {ok, P} = oscilloscope_cache_sup:spawn_cache(Metric),
            P;
        P -> P
    end,
    gen_server:cast(Pid, {process, Timestamp, Value}).

start_link(Metric) ->
    gen_server:start_link(?MODULE, Metric, []).

init(Metric) ->
    gproc:reg({n, l, Metric}, ignored),
    {ok, #st{metric=Metric, datapoints=[]}}.

handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, error, State}.

handle_cast({process, Timestamp, Value}, #st{datapoints=DP}=State) ->
    %% TODO: store in redis, not in state
    {noreply, State#st{datapoints=[{Timestamp, Value}|DP]}};
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(timeout, State) ->
    {noreply, maybe_persist_datapoints(State)};
handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

maybe_persist_datapoints(State) ->
    % TODO
    State.
