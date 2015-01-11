-module(osc_cache).
-behaviour(gen_server).

-export([
    start_link/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    start/1,
    find/1,
    read/3,
    update/2
]).

-include_lib("osc/include/osc_types.hrl").

%% This data structure is a gb_tree to ensure stable sorting when selecting
%% a window for querying. It also has values/1, and dict doesn't.
-type window_map() :: gb_trees:tree(
    osc_meta_window:window_id(),
    {osc_meta_window:windowmeta(), apod:apod()}
).

%% This data structure is a gb_tree simply because the previous one is, too.
-type persisting_map() :: gb_trees:tree(
    osc_meta_window:window_id(),
    {pid(), reference()}
).

-record(st, {
    meta :: osc_meta_metric:metricmeta(),
    windows :: window_map(),
    persisting :: persisting_map()
}).


start(Metric) ->
    case osc_meta_metric:lookup(Metric) of
        not_found -> not_found;
        {ok, Meta} -> osc_cache_sup:start_cache(Metric, Meta)
    end.


find(Metric) ->
    case gproc:where({n, l, Metric}) of
        undefined -> not_found;
        Pid -> {ok, Pid}
    end.


read(Pid, From, Until) ->
    gen_server:call(Pid, {read, From, Until}).


update(Pid, Points) ->
    gen_server:call(Pid, {update, Points}).


start_link(Metric, Meta) ->
    gen_server:start_link(?MODULE, {Metric, Meta}, []).


init({Metric, Meta}) ->
    Windows = lists:foldl(
        fun(WindowMeta, Acc) ->
            LP0 = osc_meta_window:latest_persisted_time(WindowMeta),
            LP1 = case LP0 of
                undefined ->
                    %% Apod will only accept integers
                    -1;
                _ ->
                    LP0
            end,
            {ok, Window} = apod:new(
                rect,
                osc_meta_window:aggregation(WindowMeta),
                osc_meta_window:interval(WindowMeta),
                osc_meta_window:count(WindowMeta),
                LP1
            ),
            gb_trees:insert(
                osc_meta_window:id(WindowMeta),
                {WindowMeta, Window},
                Acc
            )
        end,
        gb_trees:empty(),
        osc_meta_metric:windows(Meta)
    ),
    State = #st{
        meta=Meta,
        windows=Windows,
        persisting=gb_trees:empty()
    },
    gproc:reg({n, l, Metric}, ignored),
    Name = osc_meta_metric:name(Meta),
    % This name may be registered to another process during the metadata
    % creation process. The metadata creator should re-register this cache
    % once the metadata creation finishes.
    try gproc:reg({n, l, Name}, ignored)
    catch error:badarg ->
        ok
    end,
    {ok, State, hibernate_timeout()}.


handle_call({read, From, Until}, _From, State) ->
    #st{windows=Windows, meta=Meta}=State,
    {WindowMeta, Window} = select_window(From, gb_trees:values(Windows)),
    {ok, Read} = apod:read(Window, From, Until),
    {reply, {ok, Meta, WindowMeta, Read}, State, hibernate_timeout()};
handle_call({update, Points}, _From, State0) ->
    #st{windows=Windows0, persisting=Persisting0}=State0,
    %folsom_metrics:notify({osc, cache_updates}, {inc, 1}),
    Windows1 = gb_trees:map(
        fun(_ID, {WindowMeta, Window}) ->
            lists:foreach(
                fun({T, V}) ->
                    apod:update(Window, T, V)
                end,
                Points
            ),
            {WindowMeta, Window}
        end,
        Windows0
    ),
    Persisting1 = maybe_persist(Windows1, Persisting0),
    State1 = State0#st{windows=Windows1, persisting=Persisting1},
    {reply, ok, State1, hibernate_timeout()};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, error, State}.


handle_cast({update, Points}, State0) ->
    #st{windows=Windows0, persisting=Persisting0}=State0,
    %folsom_metrics:notify({osc, cache_updates}, {inc, 1}),
    Windows1 = gb_trees:map(
        fun(_ID, {WindowMeta, Window}) ->
            lists:foreach(
                fun({T, V}) ->
                    apod:update(Window, T, V)
                end,
                Points
            ),
            {WindowMeta, Window}
        end,
        Windows0
    ),
    Persisting1 = maybe_persist(Windows1, Persisting0),
    State1 = State0#st{windows=Windows1, persisting=Persisting1},
    {noreply, State1, hibernate_timeout()};
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info({update, Points}, State0) ->
    #st{windows=Windows0, persisting=Persisting0}=State0,
    %folsom_metrics:notify({osc, cache_updates}, {inc, 1}),
    Windows1 = gb_trees:map(
        fun(_ID, {WindowMeta, Window}) ->
            lists:foreach(
                fun({T, V}) ->
                    apod:update(Window, T, V)
                end,
                Points
            ),
            {WindowMeta, Window}
        end,
        Windows0
    ),
    Persisting1 = maybe_persist(Windows1, Persisting0),
    State1 = State0#st{windows=Windows1, persisting=Persisting1},
    {noreply, State1, hibernate_timeout()};
handle_info(timeout, State) ->
    #st{windows=Windows, persisting=Persisting0} = State,
    Persisting1 = maybe_persist(Windows, Persisting0),
    case gb_trees:is_empty(Persisting1) of
        true ->
            %% No windows are currently persisting, and nothing is worth trying
            %% to persist, so go to sleep.
            case Persisting0 =/= Persisting1 of
                true ->
                    %% This should never happen - maybe_persist should only be
                    %% able to add items to the list.
                    lager:critical(
                        "Bad persist mutation; old was ~p, new is ~p",
                        [Persisting0, Persisting1]
                    );
                false ->
                    ok
            end,
            {noreply, State#st{persisting=Persisting1}, hibernate};
        false ->
            %% Some persist attempts are in-progress; don't hibernate.
            {noreply, State#st{persisting=Persisting1}}
    end;
handle_info({'DOWN', Ref, process, Pid, Reason}=Msg, State0) ->
    #st{windows=Windows0, persisting=Persisting0} = State0,
    Key = {Pid, Ref},
    case gb_trees:lookup(Key, Persisting0) of
        none ->
            %% Log a critical message, but don't crash - it's somewhat expensive
            %% to recreate the state of this server.
            lager:critical(
                "Cache received DOWN message from unknown pid: ~p",
                [Msg]
            ),
            {noreply, State0};
        {value, WindowID} ->
            Persisting1 = gb_trees:delete(Key, Persisting0),
            State1 = case Reason of
                normal ->
                    %% Persist was successful - update our internal state
                    {WindowMeta0, Window} = gb_trees:get(WindowID, Windows0),
                    WindowMeta1 = osc_meta_window:refresh(WindowMeta0),
                    LP0 = osc_meta_window:latest_persisted_time(WindowMeta1),
                    LP1 = case LP0 of
                        undefined ->
                            %% Apod will only accept integers
                            -1;
                        _ ->
                            LP0
                    end,
                    ok = apod:truncate(Window, LP1),
                    Windows1 = gb_trees:enter(
                        WindowID,
                        {WindowMeta1, Window},
                        Windows0
                    ),
                    State0#st{windows=Windows1, persisting=Persisting1};
                _ ->
                    lager:warning(
                        "Persistence process crashed with reason ~p", [Reason]
                    ),
                    State0
            end,
            {noreply, State1#st{persisting=Persisting1}, hibernate_timeout()}
    end;
handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


select_window(From, Windows) ->
    [W|Ws] = lists:sort(
        fun({MetaA, _}, {MetaB, _}) ->
            IntervalA = osc_meta_window:interval(MetaA),
            IntervalB = osc_meta_window:interval(MetaB),
            IntervalA >= IntervalB
        end,
        Windows
    ),
    lists:foldl(
        fun(Window, Selected) ->
            EarliestTime = earliest_timestamp(Window),
            case EarliestTime =/= undefined andalso EarliestTime =< From of
                true -> Window;
                false -> Selected
            end
        end,
        W,
        Ws
    ).


-spec earliest_timestamp({Meta, Window}) -> Timestamp when
    Meta :: osc_meta_window:window(),
    Window :: apod:apod(),
    Timestamp :: timestamp() | undefined.

earliest_timestamp({Meta, Window}) ->
    case osc_meta_window:earliest_persisted_time(Meta) of
        undefined -> apod:earliest_time(Window);
        Timestamp -> Timestamp
    end.


hibernate_timeout() ->
    {ok, Timeout} = application:get_env(osc, cache_hibernate_timeout),
    Timeout.


-spec maybe_persist(Windows, Persisting) -> Persisting when
    Windows :: window_map(),
    Persisting :: persisting_map().

maybe_persist(Windows, Persisting) ->
    case gb_trees:is_empty(Persisting) of
        true ->
            lists:foldl(
                fun({WindowID, Thunk}, Acc) ->
                    Key = spawn_monitor(Thunk),
                    gb_trees:insert(Key, WindowID, Acc)
                end,
                gb_trees:empty(),
                thunk_persists(Windows)
            );
        false ->
            Persisting
    end.

-spec thunk_persists(Windows) -> Thunks when
    Windows :: window_map(),
    Thunks :: [{osc_meta_window:window_id(), fun(() -> ok)}].

thunk_persists(Windows) ->
    {ok, Threshold} = application:get_env(osc, chunkifyability_threshold),
    Iterator = gb_trees:iterator(Windows),
    thunk_persists(gb_trees:next(Iterator), Threshold, []).

thunk_persists(none, _Threshold, Thunks) ->
    Thunks;
thunk_persists({WindowID, {WindowMeta, Window}, Iterator}, Threshold, Thunks0) ->
    Thunks1 = case apod:chunkifyability(Window) > Threshold of
        true ->
            Thunk = fun() ->
                ok = osc_persistence:persist(WindowMeta, Window),
                ok = osc_persistence:vacuum(WindowMeta, Window)
            end,
            [{WindowID, Thunk}|Thunks0];
        false ->
            Thunks0
    end,
    thunk_persists(gb_trees:next(Iterator), Threshold, Thunks1).
