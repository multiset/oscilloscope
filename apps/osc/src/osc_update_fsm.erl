-module(osc_update_fsm).
-behavior(gen_fsm).

-compile([{parse_transform, lager_transform}]).

-export([
    update/3
]).

-export([
    prepare/2,
    execute/2,
    waiting/2
]).

-export([
    start_link/5,
    init/1,
    handle_event/3,
    handle_info/3,
    handle_sync_event/4,
    code_change/4,
    terminate/3
]).

-record(st, {
    bucket :: riak_core_bucket_type:bucket_type_props(),
    metric :: metric(),
    points :: [{timestamp(), value()}],
    preflist :: riak_core_apl:preflist(),
    replies :: [ok],
    req_id :: integer(),
    sender :: pid(),
    w :: pos_integer(),
    timeout :: pos_integer()
}).

-include("osc.hrl").
-include_lib("osc/include/osc_types.hrl").


-spec update(Metric, Points, Opts) -> {ok, ReqID} when
    Metric :: metric(),
    Points :: [{timestamp(), value()}],
    Opts :: [{atom(), any()}],
    ReqID :: integer().

update(Metric, Points, Opts) ->
    %% TODO: consider using a ref here
    ReqID = erlang:phash2(erlang:now()),
    lager:debug(
        "Processing update: ~p ~p",
        [Metric, Points]
    ),
    osc_update_fsm_sup:spawn([ReqID, self(), Metric, Points, Opts]),
    {ok, ReqID}.

start_link(ReqID, Sender, Metric, Points, Opts) ->
    gen_fsm:start_link(?MODULE, [ReqID, Sender, Metric, Points, Opts], []).

init([ReqID, Sender, Metric, Points, Opts]) ->
    case osc_util:ring_ready() of
        false ->
            {stop, ring_not_ready};
        true ->
            Bucket = riak_core_bucket:get_bucket(
                proplists:get_value(bucket, Opts, ?DEFAULT_BUCKET)
            ),
            State = #st{
                w = proplists:get_value(w, Opts, ?DEFAULT_W),
                bucket = Bucket,
                replies = [],
                req_id = ReqID,
                sender = Sender,
                metric = Metric,
                points = Points,
                timeout = proplists:get_value(timeout, Opts, ?DEFAULT_TIMEOUT)
            },
            {ok, prepare, State, 0}
    end.

prepare(timeout, #st{bucket=Bucket, metric=Metric}=State) ->
    Key = riak_core_util:chash_key({Bucket, Metric}),
    N = riak_core_bucket:n_val(Bucket),
    Preflist = riak_core_apl:get_apl(Key, N, osc),
    {next_state, execute, State#st{preflist=Preflist}, 0}.

execute(timeout, State) ->
    ok = osc_vnode:update(
        State#st.preflist,
        State#st.req_id,
        State#st.metric,
        State#st.points
    ),
    {next_state, waiting, State, State#st.timeout}.

waiting(timeout, State) ->
    lager:warning(
        "Update FSM timed out while waiting for cache for metric ~p",
        [State#st.metric]
    ),
    {stop, timeout, State};
waiting({ok, ReqID, Reply}, State0) ->
    #st{
        w=W,
        replies=Replies0,
        sender=Sender,
        timeout=Timeout
    } = State0,
    Replies1 = [Reply|Replies0],
    State1 = State0#st{replies=Replies1},
    case length(Replies1) >= W of
        true ->
            Sender ! {ReqID, ok},
            {stop, normal, State1};
        false ->
            {next_state, waiting, State1, Timeout}
    end.

handle_info(Msg, _StateName, StateData) ->
    {stop, {unknown_info, Msg}, StateData}.

handle_event(Event, _StateName, StateData) ->
    {stop, {unknown_event, Event}, StateData}.

handle_sync_event(Event, _From, _StateName, StateData) ->
    {stop, {unknown_sync_event, Event}, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

terminate(_Reason, _SN, _SD) ->
    ok.
