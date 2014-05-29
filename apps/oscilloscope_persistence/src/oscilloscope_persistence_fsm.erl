-module(oscilloscope_persistence_fsm).
-behavior(gen_fsm).

-export([
    fold_vnode/2,
    wait_for_fold/2,
    lock_metric/2,
    wait_for_lock/2,
    persist/2,
    vacuum/2,
    refresh_cache/2,
    wait_for_refresh/2,
    reply/2
]).

-export([
    start_link/3,
    init/1,
    handle_event/3,
    handle_info/3,
    handle_sync_event/4,
    code_change/4,
    terminate/3
]).

-record(st, {
    r,
    req_id,
    sender,
    preflist,
    bucket,
    lock_replies = [],
    fold_replies = [],
    refresh_replies = [],
    metric,
    resolution,
    cache,
    to_persist,
    to_vacuum
}).

-include_lib("oscilloscope/include/oscilloscope.hrl").
-include_lib("oscilloscope/include/oscilloscope_types.hrl").

start_link(ReqID, Sender, Opts) ->
    gen_fsm:start_link(?MODULE, [ReqID, Sender, Opts]).

init([ReqID, Sender, Opts]) ->
    State = #st{
        r = proplists:get_value(r, Opts, ?DEFAULT_R),
        req_id = ReqID,
        sender = Sender,
        bucket = proplists:get_value(bucket, Opts, ?DEFAULT_BUCKET)
    },
    {ok, fold_vnode, State, 0}.

fold_vnode(timeout, #st{req_id=ReqID, r=R, bucket=Bucket}=State) ->
    Key = riak_core_util:chash_key({Bucket, term_to_binary(now())}),
    Preflist = riak_core_apl:get_apl(Key, R, oscilloscope),
    oscilloscope_vnode:fold(
        Preflist,
        ReqID,
        fun vnode_fold/4,
        nil
    ),
    {next_state, wait_for_fold, State#st{preflist=Preflist}}.

wait_for_fold({ok, _ReqID, {ok, Metric}=Reply}, State0) ->
    #st{r=R, fold_replies=Replies0} = State0,
    Replies1 = [Reply|Replies0],
    State1 = State0#st{fold_replies=Replies1},
    case length(Replies1) >= R of
        true ->
            case lists:usort(Replies1) of
                [{ok, {Metric, Cache, Resolution, ToPersist, ToVacuum}}] ->
                    State2 = State1#st{
                        metric=Metric,
                        cache=Cache,
                        resolution=Resolution,
                        to_persist=ToPersist,
                        to_vacuum=ToVacuum
                    },
                    {next_state, execute_lock, State2, 0};
                [{ok, {Metric, Cache, Resolution, ToPersist, ToVacuum}}|_] ->
                    %% TODO: Read repair
                    State2 = State1#st{
                        metric=Metric,
                        cache=Cache,
                        resolution=Resolution,
                        to_persist=ToPersist,
                        to_vacuum=ToVacuum
                    },
                    {next_state, lock_metric, State2, 0};
                [{ok, nil}] ->
                    {stop, no_suitable_metrics, State1}
            end;
        false ->
            {next_state, wait_for_fold, State1}
    end.

lock_metric(timeout, State) ->
    #st{
        req_id=ReqID,
        preflist=Preflist,
        metric=Metric
    } = State,
    oscilloscope_vnode:lock(
        Preflist,
        ReqID,
        Metric
    ),
    {next_state, wait_for_lock, State}.

wait_for_lock({ok, _ReqID, {ok, locked}=Reply}, State0) ->
    #st{r=R, lock_replies=Replies0} = State0,
    Replies1 = [Reply|Replies0],
    State1 = State0#st{lock_replies=Replies1},
    case length(Replies1) >= R of
        true ->
            %% Assert that all replies match and are locks
            [{ok, locked}] = lists:usort(Replies1),
            {next_state, persist, State1, 0};
        false ->
            {next_state, wait_for_lock, State1}
    end;
wait_for_lock({ok, _ReqID, Reply}, State) ->
    %% TODO: log
    {stop, {lock_failed, Reply}, State}.

persist(timeout, State) ->
    #st{
        resolution=Resolution,
        to_persist=ToPersist
    } = State,
    {ok, _Persisted} = oscilloscope_persistence:persist(
        Resolution,
        ToPersist
    ),
    {next_state, vacuum, State, 0}.

vacuum(timeout, State) ->
    #st{
        resolution=Resolution,
        to_vacuum=ToVacuum
    } = State,
    {ok, _Vacuumed} = oscilloscope_persistence:vacuum(
        Resolution,
        ToVacuum
    ),
    {next_state, refresh_cache, State, 0}.

refresh_cache(timeout, State) ->
    #st{
        req_id=ReqID,
        preflist=Preflist,
        metric=Metric
    } = State,
    oscilloscope_vnode:refresh(
        Preflist,
        ReqID,
        Metric
    ),
    {next_state, wait_for_refresh, State}.

wait_for_refresh({ok, _ReqID, Reply}, State0) ->
    #st{r=R, refresh_replies=Replies0}=State0,
    Replies1 = [Reply|Replies0],
    State1 = State0#st{refresh_replies=Replies1},
    case length(Replies1) >= R of
        true ->
            {next_state, reply, State1, 0};
        false ->
            {next_state, wait_for_persist_update, State1}
    end.

reply(timeout, #st{sender=Sender, req_id=ReqID}=State) ->
    Sender ! {ReqID, ok},
    {stop, normal, State}.

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

vnode_fold(_Meta, _Cache, _Points, Acc) ->
    Acc.
