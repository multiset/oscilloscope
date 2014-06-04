-module(oscilloscope_update_fsm).
-behavior(gen_fsm).

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
    w,
    bucket,
    replies,
    req_id,
    preflist,
    sender,
    metric,
    points
}).

-include("oscilloscope.hrl").

update(Metric, Points, Opts) ->
    %% TODO: consider using a ref here
    ReqID = erlang:phash2(erlang:now()),
    lager:debug(
        "Processing update: ~p ~p",
        [Metric, Points]
    ),
    oscilloscope_update_fsm_sup:spawn([ReqID, self(), Metric, Points, Opts]),
    {ok, ReqID}.

start_link(ReqID, Sender, Metric, Points, Opts) ->
    gen_fsm:start_link(?MODULE, [ReqID, Sender, Metric, Points, Opts], []).

init([ReqID, Sender, Metric, Points, Opts]) ->
    case oscilloscope_util:ring_ready() of
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
                points = Points
            },
            {ok, prepare, State, 0}
    end.

prepare(timeout, #st{bucket=Bucket, metric=Metric}=State) ->
    Key = riak_core_util:chash_key({Bucket, Metric}),
    N = riak_core_bucket:n_val(Bucket),
    Preflist = riak_core_apl:get_apl(Key, N, oscilloscope),
    {next_state, execute, State#st{preflist=Preflist}, 0}.

execute(timeout, State) ->
    oscilloscope_vnode:update(
        State#st.preflist,
        State#st.req_id,
        State#st.metric,
        State#st.points
    ),
    {next_state, waiting, State}.

waiting({ok, ReqID, Reply}, #st{w=W, replies=Replies0, sender=Sender}=State0) ->
    Replies1 = [Reply|Replies0],
    State1 = State0#st{replies=Replies1},
    case length(Replies1) >= W of
        true ->
            Sender ! {ReqID, ok},
            {stop, normal, State1};
        false ->
            {next_state, waiting, State1}
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
