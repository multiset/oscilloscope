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
    update/2,
    persist/1
]).

-include_lib("osc/include/osc_types.hrl").

-type window() :: {osc_meta_window:window(), apod:apod()}.
-type persist() :: {osc_meta_window:window_id(), {pid(), reference()}}.

-record(st, {
    meta :: osc_meta_metric:metricmeta(),
    windows :: [window()],
    persisting :: [persist()]
}).


-spec start(Metric) -> {ok, Cache} | {error, Error} when
    Metric :: term(),
    Cache :: pid(),
    Error :: not_found | unknown.

start(Metric) ->
    Lookup = try osc_meta_metric:lookup(Metric)
    catch error:{badmatch, B} ->
        lager:warning("badmatch in osc_cache:start/1: ~p", [B]),
        unknown
    end,
    case Lookup of
        {ok, Meta} -> osc_cache_sup:start_cache(Metric, Meta);
        Else -> {error, Else}
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


persist(Pid) ->
    gen_server:call(Pid, persist).


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
            {ok, WindowData} = apod:new(
                rect,
                osc_meta_window:aggregation(WindowMeta),
                osc_meta_window:interval(WindowMeta),
                osc_meta_window:count(WindowMeta),
                LP1
            ),
            [{WindowMeta, WindowData}|Acc]
        end,
        [],
        osc_meta_metric:windows(Meta)
    ),
    State = #st{
        meta=Meta,
        windows=Windows,
        persisting=[]
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
    {WindowMeta, WindowData} = select_window(From, Windows),
    {ok, Read} = apod:read(WindowData, From, Until),
    {reply, {ok, Meta, WindowMeta, Read}, State, hibernate_timeout()};
handle_call({update, Points}, _From, State) ->
    #st{windows=Windows, persisting=Persisting}=State,
    UpdateCount = length(Points) * length(Windows),
    folsom_metrics:notify({osc, cache_updates}, {inc, UpdateCount}),
    lists:foreach(
        fun({_WMeta, WData}) ->
            ok = apod:update(WData, Points)
        end,
        Windows
    ),
    {ok, Threshold} = application:get_env(osc, chunkifyability_threshold),
    case {Persisting, maybe_persist(Windows, Persisting, Threshold)} of
        {[], []} ->
            {reply, ok, State, hibernate_timeout()};
        {P0, P1} ->
            {reply, ok, State#st{persisting=P1 ++ P0}}
    end;
handle_call(persist, _From, State) ->
    #st{windows=W, persisting=P0} = State,
    P1 = lists:filtermap(
        fun({WMeta, _WData}=Window) ->
            ID = osc_meta_window:id(WMeta),
            case lists:keyfind(ID, 1, P0) of
                false ->
                    {true, spawn_persist(Window)};
                _ ->
                    false
            end
        end,
        W
    ),
    {reply, ok, State#st{persisting=P0 ++ P1}};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, error, State}.


handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.


handle_info(timeout, #st{persisting=P}=State) when length(P) =/= 0 ->
    %% There are outstanding persists; don't hibernate
    {noreply, State};
handle_info(timeout, State) ->
    {ok, Threshold} = application:get_env(osc, chunkifyability_threshold),
    case maybe_persist(State#st.windows, [], Threshold) of
        [] ->
            %% No windows are currently persisting, and nothing is worth trying
            %% to persist, so go to sleep.
            {noreply, State, hibernate};
        Persisting ->
            %% Some persist attempts are in-progress; don't hibernate.
            {noreply, State#st{persisting=Persisting}}
    end;
handle_info({'DOWN', Ref, process, Pid, Reason}=Msg, State0) ->
    #st{windows=Windows0, persisting=Persisting0} = State0,
    Key = {Pid, Ref},
    case lists:keytake(Key, 2, Persisting0) of
        false ->
            %% Log a critical message, but don't crash - it's somewhat expensive
            %% to recreate the state of this server.
            lager:critical(
                "Cache received DOWN message from unknown pid: ~p",
                [Msg]
            ),
            {noreply, State0};
        {value, {WindowID, Key}, Persisting1} ->
            State1 = case Reason of
                normal ->
                    %% Persist was successful - update our internal state
                    Windows1 = lists:foldl(
                        fun({WMeta0, WData}=W, Acc) ->
                            case osc_meta_window:id(WMeta0) =:= WindowID of
                                false ->
                                    [W|Acc];
                                true ->
                                    WMeta1 = osc_meta_window:refresh(WMeta0),
                                    LP0 = osc_meta_window:latest_persisted_time(
                                        WMeta1
                                    ),
                                    LP1 = case LP0 of
                                        undefined ->
                                            %% This shouldn't happen
                                            lager:critical(
                                                "Post-persist latest persisted "
                                                "time was undefined for ~p",
                                                [WindowID]
                                            ),
                                            -1;
                                        _ ->
                                            LP0
                                    end,
                                    ok = apod:truncate(WData, LP1),
                                    [{WMeta1, WData}|Acc]
                            end
                        end,
                        [],
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
            case IntervalA =:= IntervalB of
                true ->
                    osc_meta_window:id(MetaA) >= osc_meta_window:id(MetaB);
                false ->
                    IntervalA >= IntervalB
            end
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


-spec maybe_persist(Windows, Persisting, Threshold) -> Persisting when
    Windows :: [window()],
    Persisting :: [persist()],
    Threshold :: float().

maybe_persist(Windows, Persisting, Threshold) ->
    lists:filtermap(
        fun({WMeta, WData}=W) ->
            ID = osc_meta_window:id(WMeta),
            case lists:keyfind(ID, 1, Persisting) of
                false ->
                    AverageSize = osc_meta_window:average_persist_size(WMeta),
                    case apod:size(WData)/AverageSize > Threshold of
                        true ->
                            {true, spawn_persist(W)};
                        false ->
                            false
                    end;
                _ ->
                    false
            end
        end,
        Windows
    ).


-spec spawn_persist(Window) -> Persist when
    Window :: window(),
    Persist :: persist().

spawn_persist({WindowMeta, WindowData}) ->
    Ref = spawn_monitor(fun() ->
        ok = osc_persistence:persist(WindowMeta, WindowData),
        ok = osc_persistence:vacuum(WindowMeta, WindowData)
    end),
    {osc_meta_window:id(WindowMeta), Ref}.
