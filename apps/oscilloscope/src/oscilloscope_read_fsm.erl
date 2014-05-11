-module(oscilloscope_read_fsm).
-behavior(gen_fsm).

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
    bucket :: binary(),
    cache_read :: read(),
    from :: timestamp(),
    meta :: oscilllscope_metadata:meta(),
    metric :: metric(),
    persistent_read :: read(),
    preflist :: riak_core_apl:preflist(),
    r :: pos_integer(),
    replies :: [read() | not_found],
    reply :: any(),
    req_id :: integer(),
    resolution :: resolution(),
    sender :: pid(),
    until :: timestamp()
}).

-include("oscilloscope.hrl").
-include_lib("oscilloscope/include/oscilloscope_types.hrl").

read(Metric, From, Until, Opts) ->
    ReqID = erlang:phash2(erlang:now()),
    lager:debug(
        "Processing read: ~p ~p ~p",
        [Metric, From, Until]
    ),
    oscilloscope_read_fsm_sup:spawn([ReqID, self(), Metric, From, Until, Opts]),
    {ok, ReqID}.

start_link(ReqID, Sender, Metric, From, Until, Opts) ->
    gen_fsm:start_link(?MODULE, [ReqID, Sender, Metric, From, Until, Opts], []).

init([ReqID, Sender, Metric, From, Until, Opts]) ->
    State0 = #st{
        r = proplists:get_value(r, Opts, ?DEFAULT_R),
        bucket = proplists:get_value(bucket, Opts, ?DEFAULT_BUCKET),
        replies = [],
        req_id = ReqID,
        sender = Sender,
        metric = Metric,
        from = From,
        until = Until
    },
    case oscilloscope_metadata:find(Metric) of
        {error, Error} ->
            {ok, reply, State0#st{reply=Error}, 0};
        {ok, Meta} ->
            State1 = State0#st{meta=Meta},
            {ok, prepare_cache_read, State1, 0}
    end.

prepare_cache_read(timeout, #st{r=R, bucket=Bucket, metric=Metric}=State) ->
    Key = riak_core_util:chash_key({Bucket, Metric}),
    Preflist = riak_core_apl:get_apl(Key, R, oscilloscope),
    {next_state, execute_cache_read, State#st{preflist=Preflist}, 0}.

execute_cache_read(timeout, State) ->
    oscilloscope_vnode:read(
        State#st.preflist,
        State#st.req_id,
        State#st.metric,
        State#st.from,
        State#st.until
    ),
    {next_state, wait_for_cache, State}.

wait_for_cache({ok, _ReqID, Reply}, State0) ->
    #st{
        r=R,
        from=From,
        replies=Replies0,
        meta=Meta
    } = State0,
    Replies1 = [Reply|Replies0],
    State1 = State0#st{replies=Replies1},
    case length(Replies1) >= R of
        true ->
            Read = case lists:usort(Replies1) of
                [Reply] -> Reply;
                _Else -> hd(Replies1) %% TODO read repair
            end,
            {CFrom, CUntil, Resolution, Points} = Read,
            State2 = State1#st{
                cache_read=Read,
                resolution=Resolution
            },
            case CFrom =< From of
                true ->
                    Response = [
                        {meta, Meta},
                        {from, CFrom},
                        {until, CUntil},
                        {resolution, Resolution},
                        {datapoints, Points}
                    ],
                    {next_state, reply, State2#st{reply=Response}, 0};
                false ->
                    {next_state, execute_persistent_read, State2, 0}
            end;
        false ->
            {next_state, wait_for_cache, State1}
    end.

execute_persistent_read(timeout, State) ->
    #st{
        resolution=Resolution,
        from=From,
        until=Until
    } = State,
    {ok, Read} = oscilloscope_persistence:read(Resolution, From, Until),
    {next_state, merge_reads, State#st{persistent_read=Read}, 0}.

merge_reads(timeout, State) ->
    #st{
        meta=Meta,
        cache_read=CRead,
        persistent_read=PRead,
        resolution={_ID, Interval, _Count, _Persisted}=Resolution
    } = State,
    {CFrom, CUntil, Resolution, CData} = CRead,
    {PFrom, PUntil, Resolution, PData} = PRead,
    Gap = lists:duplicate((CFrom - PUntil) div Interval, null),
    Reply = [
        {meta, Meta},
        {from, PFrom},
        {until, CUntil},
        {resolution, Resolution},
        {datapoints, PData ++ Gap ++ CData}
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
