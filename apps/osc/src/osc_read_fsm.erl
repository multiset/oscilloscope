-module(osc_read_fsm).
-behavior(gen_fsm).

-compile([{parse_transform, lager_transform}]).

-export([
    read/4
]).

-export([
    prepare_cache_read/2,
    execute_cache_read/2,
    wait_for_cache/2,
    execute_persistent_read/2,
    merge_reads/2,
    reply/2
]).

-export([
    start_link/6,
    init/1,
    handle_event/3,
    handle_info/3,
    handle_sync_event/4,
    code_change/4,
    terminate/3
]).

-record(st, {
    bucket :: riak_core_bucket_type:bucket_type_props(),
    cache_read :: read(),
    from :: timestamp(),
    meta :: oscilllscope_meta:meta(),
    metric :: metric(),
    persistent_read :: read() | not_found,
    preflist :: riak_core_apl:preflist(),
    r :: pos_integer(),
    replies :: [{ok, cache_read()} | {error, atom()}],
    reply :: any(),
    req_id :: integer(),
    resolution :: osc_meta_resolution:resolution(),
    sender :: pid(),
    until :: timestamp(),
    timeout :: pos_integer()
}).

-include("osc.hrl").
-include_lib("osc/include/osc_types.hrl").


-spec read(Metric, From, Until, Opts) -> {ok, ReqID} when
    Metric :: metric(),
    From :: timestamp(),
    Until :: timestamp(),
    Opts :: [{atom(), any()}],
    ReqID :: integer().

read(Metric, From, Until, Opts) ->
    %% TODO: consider using a ref here
    ReqID = erlang:phash2(erlang:now()),
    lager:debug(
        "Processing read: ~p ~p ~p",
        [Metric, From, Until]
    ),
    osc_read_fsm_sup:spawn([ReqID, self(), Metric, From, Until, Opts]),
    {ok, ReqID}.

start_link(ReqID, Sender, Metric, From, Until, Opts) ->
    gen_fsm:start_link(?MODULE, [ReqID, Sender, Metric, From, Until, Opts], []).

init([ReqID, Sender, Metric, From, Until, Opts]) ->
    case osc_util:ring_ready() of
        false ->
            {stop, ring_not_ready};
        true ->
            Bucket = riak_core_bucket:get_bucket(
                proplists:get_value(bucket, Opts, ?DEFAULT_BUCKET)
            ),
            State = #st{
                r = proplists:get_value(r, Opts, ?DEFAULT_R),
                bucket = Bucket,
                replies = [],
                req_id = ReqID,
                sender = Sender,
                metric = Metric,
                from = From,
                until = Until,
                timeout = proplists:get_value(timeout, Opts, ?DEFAULT_TIMEOUT)
            },
            {ok, prepare_cache_read, State, 0}
    end.

prepare_cache_read(timeout, #st{bucket=Bucket, metric=Metric}=State) ->
    Key = riak_core_util:chash_key({Bucket, Metric}),
    N = riak_core_bucket:n_val(Bucket),
    Preflist = riak_core_apl:get_apl(Key, N, osc),
    {next_state, execute_cache_read, State#st{preflist=Preflist}, 0}.

execute_cache_read(timeout, State) ->
    ok = osc_vnode:read(
        State#st.preflist,
        State#st.req_id,
        State#st.metric,
        State#st.from,
        State#st.until
    ),
    {next_state, wait_for_cache, State, State#st.timeout}.

wait_for_cache(timeout, State) ->
    lager:warning(
        "Read FSM timed out while waiting for cache for metric ~p",
        [State#st.metric]
    ),
    {stop, timeout, State};
wait_for_cache({ok, _ReqID, Reply}, State0) ->
    #st{
        r=R,
        replies=Replies0,
        timeout=Timeout
    } = State0,
    Replies1 = [Reply|Replies0],
    State1 = State0#st{replies=Replies1},
    case length(Replies1) >= R of
        true ->
            Read = case lists:usort(Replies1) of
                [Reply] -> Reply;
                _Else -> hd(Replies1) %% TODO read repair
            end,
            case Read of
                {error, _Error} ->
                    {next_state, reply, State1#st{reply=Read}};
                {ok, {Meta, Resolution, CRead}} ->
                    State2 = State1#st{
                        cache_read=CRead,
                        meta=Meta,
                        resolution=Resolution
                    },
                    {next_state, execute_persistent_read, State2, 0}
            end;
        false ->
            {next_state, wait_for_cache, State1, Timeout}
    end.

execute_persistent_read(timeout, State) ->
    #st{
        resolution=Resolution,
        from=From,
        until=Until
    } = State,
    %% TODO: handle timeout/make sure we don't wait forever
    {ok, Read} = osc_persistence:read(Resolution, From, Until),
    {next_state, merge_reads, State#st{persistent_read=Read}, 0}.

merge_reads(timeout, State) ->
    #st{
        meta=Meta,
        cache_read=CRead,
        persistent_read=PRead,
        resolution=Resolution,
        from=From0,
        until=Until0
    } = State,
    Interval = osc_meta_resolution:interval(Resolution),
    {From1, Until1} = osc_util:adjust_query_range(
        From0,
        Until0,
        Interval
    ),
    Points = case {PRead, CRead} of
        {not_found, not_found} ->
            lists:duplicate((Until1 - From1) div Interval, null);
        {{PFrom, PUntil, PData}, not_found} ->
            lists:append([
                lists:duplicate((PFrom - From1) div Interval, null),
                PData,
                lists:duplicate((Until1 - PUntil) div Interval, null)
            ]);
        {not_found, {CFrom, CUntil, CData}} ->
            lists:append([
                lists:duplicate((CFrom - From1) div Interval, null),
                CData,
                lists:duplicate((Until1 - CUntil) div Interval, null)
            ]);
        {{_, PUntil, PData}, {CFrom, CUntil, CData}} ->
            lists:append([
                lists:duplicate((CFrom - From1) div Interval, null),
                PData,
                lists:duplicate((CFrom - PUntil) div Interval, null),
                CData,
                lists:duplicate((Until1 - CUntil) div Interval, null)
            ])
    end,
    Reply = [
        {meta, Meta},
        {from, From1},
        {until, Until1},
        {resolution, Resolution},
        {datapoints, Points}
    ],
    {next_state, reply, State#st{reply=Reply}, 0}.

reply(timeout, #st{sender=Sender, req_id=ReqID, reply=Reply}=State) ->
    %% TODO: validate that the response covers the full range
    Sender ! {ReqID, {ok, Reply}},
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
