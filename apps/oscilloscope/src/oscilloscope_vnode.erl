-module(oscilloscope_vnode).

-behavior(riak_core_vnode).

-export([
    read/5,
    update/4,
    fold/4,
    lock/3,
    refresh/3
]).

-export([
    start_vnode/1,
    init/1,
    handle_command/3,
    handle_coverage/4,
    handle_exit/3,
    handoff_starting/2,
    handoff_cancelled/1,
    handoff_finished/2,
    handle_handoff_command/3,
    handle_handoff_data/2,
    encode_handoff_item/2,
    is_empty/1,
    terminate/2,
    delete/1
]).

-record(st, {
    metrics :: dict(),
    locks :: dict()
}).

-record(metric, {
    cache,
    lock
}).

read(Preflist, ReqID, Metric, From, Until) ->
    riak_core_vnode_master:command(
        Preflist,
        {read, ReqID, Metric, From, Until},
        {fsm, undefined, self()},
        oscilloscope_vnode_master
    ).

update(Preflist, ReqID, Metric, Points) ->
    riak_core_vnode_master:command(
        Preflist,
        {update, ReqID, Metric, Points},
        {fsm, undefined, self()},
        oscilloscope_vnode_master
    ).

fold(Preflist, ReqID, Fun, Acc) ->
    riak_core_vnode_master:command(
        Preflist,
        {fold, ReqID, Fun, Acc},
        {fsm, undefined, self()},
        oscilloscope_vnode_master
    ).

lock(Preflist, ReqID, Metric) ->
    riak_core_vnode_master:command(
        Preflist,
        {lock, ReqID, Metric},
        {fsm, undefined, self()},
        oscilloscope_vnode_master
    ).

refresh(Preflist, ReqID, Metric) ->
    riak_core_vnode_master:command(
        Preflist,
        {refresh, ReqID, Metric},
        {fsm, undefined, self()},
        oscilloscope_vnode_master
    ).

start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init(_) ->
    {ok, #st{metrics=dict:new(), locks=dict:new()}}.

handle_command({read, ReqID, Metric, From, Until}, _From, State) ->
    Metrics0 = State#st.metrics,
    case dict:find(Metric, Metrics0) of
        {ok, #metric{cache=Cache}} ->
            Read = oscilloscope_cache:read(From, Until, Cache),
            {reply, {ok, ReqID, Read}, State};
        error ->
            case oscilloscope_metadata:find(Metric) of
                {ok, Meta} ->
                    Cache = oscilloscope_cache:new(Metric, Meta),
                    Metrics1 = dict:store(
                        Metric,
                        #metric{cache=Cache},
                        Metrics0
                    ),
                    Read = oscilloscope_cache:read(From, Until, Cache),
                    {reply, {ok, ReqID, Read}, State#st{metrics=Metrics1}};
                {error, not_found} ->
                    {reply, {ok, ReqID, not_found}, State}
            end
    end;
handle_command({update, ReqID, Metric, Points}, _From, State) ->
    Metrics0 = State#st.metrics,
    Metrics1 = case dict:find(Metric, Metrics0) of
        {ok, #metric{cache=Cache0}=Stored} ->
            Cache1 = oscilloscope_cache:update(Points, Cache0),
            dict:store(Metric, Stored#metric{cache=Cache1}, Metrics0);
        error ->
            Meta = case oscilloscope_metadata:find(Metric) of
                {ok, M} ->
                    M;
                {error, not_found} ->
                    {ok, M} = oscilloscope_metadata:create(Metric),
                    M
            end,
            Cache = oscilloscope_cache:update(
                Points,
                oscilloscope_cache:new(Metric, Meta)
            ),
            dict:store(Metric, #metric{cache=Cache}, Metrics0)
    end,
    {reply, {ok, ReqID, ok}, State#st{metrics=Metrics1}};
handle_command({fold, ReqID, Fun, Acc0}, _From, #st{metrics=Metrics}=State) ->
    Acc1 = dict:fold(
        fun(Metric, #metric{cache=Cache}, A0) ->
            Fun(Metric, Cache, A0)
        end,
        Acc0,
        Metrics
    ),
    {reply, {ok, ReqID, {ok, Acc1}}, State};
handle_command({lock, ReqID, Metric}, {_, _, Pid}=_From, State0) ->
    #st{metrics=Metrics0, locks=Locks0} = State0,
    {Reply, State1} = case dict:find(Metric, Metrics0) of
        {ok, #metric{lock=undefined}=Stored} ->
            erlang:link(Pid),
            {
                {ok, locked},
                State0#st{
                    locks=dict:store(Pid, Metric, Locks0),
                    metrics=dict:store(
                        Metric,
                        Stored#metric{lock=Pid},
                        Metrics0
                    )
                }
            };
        {ok, #metric{lock=Lock}} ->
            {{error, already_locked, Lock}, State0};
        error ->
            {{error, unknown_metric}, State0}
    end,
    {reply, {ok, ReqID, Reply}, State1};
handle_command({refresh, ReqID, Metric}, _From, #st{metrics=Metrics0}=State) ->
    {Reply, Metrics1} = case dict:find(Metric, Metrics0) of
        {ok, #metric{cache=Cache0}=Stored} ->
            Cache1 = oscilloscope_cache:refresh(Cache0),
            {ok, dict:store(Metric, Stored#metric{cache=Cache1}, Metrics0)};
        error ->
            lager:error(
                "Attempt to refresh unknown metric ~p",
                [Metric]
            ),
            {{error, unknown_metric}, Metrics0}
    end,
    {reply, {ok, ReqID, Reply}, State#st{metrics=Metrics1}};
handle_command(Cmd, _From, State) ->
    {stop, {unknown_command, Cmd}, State}.

handle_coverage(_Req, _KeySpaces, _From, State) ->
    {stop, not_implemented, State}.

handoff_starting(_TargetNode, State) ->
    {false, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_command(_Cmd, _From, State) ->
    {stop, not_implemented, State}.

handle_exit(Pid, _Reason, #st{locks=Locks, metrics=Metrics}=State0) ->
    State1 = case dict:find(Pid, Locks) of
        {ok, Metric} ->
            {ok, #metric{lock=Pid}=Stored} = dict:find(Metric, Metrics),
            State0#st{
                locks=dict:erase(Pid, Locks),
                metrics=dict:store(
                    Metric,
                    Stored#metric{lock=undefined},
                    Metrics
                )
            };
        error ->
            lager:error("Vnode received exit for unknown pid: ~p", [Pid]),
            State0
    end,
    {noreply, State1}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {false, State}.

terminate(_Reason, _State) ->
    ok.

delete(State) ->
    {ok, State}.
