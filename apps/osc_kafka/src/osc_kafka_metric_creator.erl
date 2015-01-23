-module(osc_kafka_metric_creator).

-behaviour(gen_server).

-export([start_link/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(st, {
    encoded_metric,
    decoded_metric,
    datapoints,
    creator,
    flush
}).


start_link({OrgID, EncodedProps}) ->
    gen_server:start_link(?MODULE, [OrgID, EncodedProps], []).


init([OrgID, EncodedProps]) ->
    gproc:reg({n, l, {OrgID, EncodedProps}}),
    Props = osc_meta_util:decode_props(EncodedProps),
    State = #st{
        decoded_metric={OrgID, Props},
        encoded_metric={OrgID, EncodedProps},
        datapoints=[],
        creator=create_metric({OrgID, Props}),
        flush=false
    },
    {ok, State}.


handle_call({update, NewDatapoints}, _From, State) ->
    #st{
        datapoints=Datapoints
    } = State,
    NewState = State#st{datapoints=Datapoints ++ NewDatapoints},
    format_reply(ok, NewState);

% Because of gproc indirection, may get requests intended for the cache.
handle_call(_Msg, _From, State) ->
    format_reply(not_ready, State).


handle_cast(Msg, State) ->
    lager:warning(
        "osc_kafka_metric_creator ~p received unknown cast: ~p",
        [self(), Msg]
    ),
    {noreply, State}.


handle_info({'DOWN', Ref, process, Pid, Info}, #st{creator={Pid, Ref}}=State) ->
    #st{
        decoded_metric={OrgID, _}=DMetric,
        encoded_metric=EMetric
    } = State,
    case Info of
        {ok, CachePid} ->
            gproc:give_away({n, l, EMetric}, CachePid),
            {noreply, State#st{flush=true, creator=undefined}, 0};
        missing_org ->
            lager:error("Org ~p does not exist", [OrgID]),
            {stop, missing_org, State};
        _ ->
            lager:error("Metric creator failed: ~p", [Info]),
            {noreply, State#st{creator=create_metric(DMetric)}}
    end;
handle_info({'DOWN', _Ref, process, _Pid, Info}, State) ->
    lager:error(
        "Metric creator received 'DOWN' from unknown pid for reason ~p",
        [Info]
    ),
    {noreply, State};
handle_info(timeout, #st{flush=true, creator=undefined}=State) ->
    #st{
        datapoints=Datapoints,
        encoded_metric=EMetric,
        decoded_metric=DMetric
    } = State,
    case osc_cache:find(EMetric) of
        not_found ->
            lager:error(
                "metric_creator failed to create cache ~p; retrying",
                [DMetric]
            ),
            {noreply, State#st{creator=create_metric(DMetric)}};
        {ok, Pid} when Pid =/= self() ->
            ok = osc_cache:update(Pid, Datapoints),
            {stop, normal, State};
        _ ->
            lager:error(
                "metric_creator registered after creation for ~p; retrying",
                [DMetric]
            ),
            {noreply, State#st{creator=create_metric(DMetric)}}
    end;
handle_info(Msg, State) ->
    lager:error(
        "metric_creator received unknown info ~p in state ~p",
        [Msg, State]
    ),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


create_metric(Metric) ->
    spawn_monitor(fun() ->
        case osc_meta_metric:create(Metric) of
            {ok, _MetricID} ->
                exit(osc_cache:start(Metric));
            {error, exists} ->
                exit(osc_cache:start(Metric));
            {error, missing_org} ->
                exit(missing_org)
        end
    end).


format_reply(Reply, State) ->
    case State of
        #st{flush=true} ->
            {reply, Reply, State, 0};
        #st{flush=false} ->
            {reply, Reply, State}
    end.
