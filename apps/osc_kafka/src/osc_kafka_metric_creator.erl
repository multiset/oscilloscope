-module(osc_kafka_metric_creator).

-behaviour(gen_server).

-export([start_link/2]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    metric,
    datapoints,
    flush=false
}).


start_link(OwnerID, EncodedProps) ->
    gen_server:start_link(?MODULE, [OwnerID, EncodedProps], []).


init([OwnerID, EncodedProps]) ->
    gproc:reg({n, l, {OwnerID, EncodedProps}}),
    spawn_monitor(fun() ->
        create_metric({OwnerID, EncodedProps})
    end),
    {ok, #state{metric={OwnerID, EncodedProps}}, 0}.


handle_call({update, NewDatapoints}, _From, State) ->
    #state{
        datapoints=Datapoints
    } = State,
    NewState = State#state{datapoints=[NewDatapoints|Datapoints]},
    format_reply(ok, NewState);

% Because of gproc indirection, may get requests intended for the cache.
handle_call(_Msg, _From, State) ->
    format_reply(not_ready, State).


handle_cast(_Msg, State) ->
    {stop, unknown_cast, State}.


handle_info({'DOWN', _, _, _, Info}, State) ->
    case Info of
        normal ->
            {noreply, State#state{flush=true}, 0};
        _ ->
            lager:error("Metric creator failed: ~p", [Info]),
            spawn_monitor(fun() ->
                create_metric(State#state.metric)
            end),
            {noreply, State}
    end;

handle_info(timeout, State) ->
    #state{
        datapoints=Datapoints,
        metric=Metric
    } = State,
    Self = self(),
    {ok, Pid} = case osc_cache:find(Metric) of
        not_found ->
            osc_cache:start(Metric);
        {ok, Pid0} ->
            {ok, Pid0}
    end,
    case Pid of
        Self ->
            {stop, cache_still_self, State};
        _ ->
            ok = osc_cache:update(Pid, Datapoints),
            {stop, normal, State}

    end.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


create_metric(Metric) ->
    case osc_meta_metric:create(Metric) of
        {ok, _MetricID} ->
            ok;
        {error, {exists, _MetricID}} ->
            ok;
        {error, Reason} ->
            lager:error("Error while creating metric: ~p", [Reason]),
            timer:sleep(5000),
            create_metric(Metric)
    end.


format_reply(Reply, State) ->
    case State of
        #state{flush=true} ->
            {reply, Reply, State, 0};
        #state{flush=false} ->
            {reply, Reply, State}
    end.
