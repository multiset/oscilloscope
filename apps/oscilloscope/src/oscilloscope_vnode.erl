-module(oscilloscope_vnode).

-behavior(riak_core_vnode).

-export([
    read/5,
    update/4
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
    metrics :: dict()
}).

-record(metric, {
    meta,
    cache
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

start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init(_) ->
    {ok, #st{metrics=dict:new()}}.

handle_command({read, ReqID, Metric, From, Until}, _From, State) ->
    Reply = case dict:find(Metric, State#st.metrics) of
        {ok, #metric{cache=Cache}} ->
            oscilloscope_cache:read(From, Until, Cache);
        error ->
            not_found
    end,
    {reply, {ok, ReqID, Reply}, State};
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
                oscilloscope_cache:new(
                    oscilloscope_metadata:aggregation(Meta),
                    oscilloscope_metadata:resolutions(Meta)
                )
            ),
            dict:store(Metric, #metric{meta=Meta, cache=Cache}, Metrics0)
    end,
    {reply, {ok, ReqID, ok}, State#st{metrics=Metrics1}};
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

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

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
