-module(osc_kafka_metric_creator).

-behaviour(gen_fsm).

-export([
    find/2,
    update/2
]).

-export([start_link/2]).

-export([
    init/1,
    create_metric/2,
    create_metric/3,
    flush/2,
    flush/3,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4
]).

-record(state, {
    datapoints=[],
    metric_id,
    owner_id,
    metric_name,
    flush_timeout=10000
}).


-spec find(OwnerID, Name) -> {ok, Pid} | undefined when
    OwnerID :: integer(),
    Name :: [{binary(), binary()}],
    Pid :: pid().

find(OwnerID, Name) ->
    gproc:where({n, l, {OwnerID, Name}}).


-spec update(Pid, Datapoints) -> ok | {error, Reason} when
    Pid :: pid(),
    Datapoints :: [{integer(), float()}],
    Reason :: any().

update(Pid, Datapoints) ->
    try gen_fsm:sync_send_event(Pid, {update, Datapoints})
    catch _:Reason ->
        {error, Reason}
    end.


start_link(OwnerID, Name) ->
    gen_fsm:start_link(?MODULE, [OwnerID, Name], []).


init([OwnerID, Name]) ->
    gproc:reg({n, l, {OwnerID, Name}}),
    {ok, create_metric, #state{owner_id=OwnerID, metric_name=Name}, 0}.


create_metric(timeout, State) ->
    #state{
        owner_id=OwnerID,
        metric_name=Name
    } = State,
    case osc_meta_metric:create({OwnerID, Name}) of
        {ok, MetricID} ->
            {next_state, flush, State#state{metric_id=MetricID}};
        {error, {exists, MetricID}} ->
            {next_state, flush, State#state{metric_id=MetricID}};
        {error, Reason} ->
            lager:error("Error while creating metric: ~p", [Reason]),
            {next_state, create_metric, State, 5000}
    end.


create_metric({update, NewDatapoints}, _From, State) ->
    #state{datapoints=Datapoints} = State,
    NewState = State#state{datapoints=[NewDatapoints|Datapoints]},
    {reply, ok, create_metric, NewState, 0}.


flush(timeout, #state{datapoints=[]}=State) ->
    {stop, normal, State};

flush(timeout, State) ->
    #state{
        datapoints=Datapoints,
        flush_timeout=Timeout,
        metric_id=MetricID
    } = State,
    ok = osc_cache:update(MetricID, lists:flatten(Datapoints)),
    {next_state, flush, State#state{datapoints=[]}, Timeout}.


flush({update, NewDatapoints}, _From, State) ->
    #state{
        datapoints=Datapoints,
        flush_timeout=Timeout,
        metric_id=MetricID
    } = State,
    ok = osc_cache:update(MetricID, lists:flatten([NewDatapoints|Datapoints])),
    {reply, ok, flush, State#state{datapoints=[]}, Timeout}.


handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.


handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.


handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.


terminate(_Reason, _StateName, State) ->
    #state{
        owner_id=OwnerID,
        metric_name=Name
    } = State,
    gproc:unreg({n, l, {OwnerID, Name}}),
    ok.


code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
