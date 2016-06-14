-module(osc_persistence_worker).

-export([
    start_link/2,
    go/3
]).

-record(st, {
    metric_id,
    window_id,
    window
}).


start_link({MetricID, WindowID, MinSize) ->
    proc_lib:start_link(?MODULE, go, [self(), MetricID, WindowID, MinSize]).


go(Parent, MetricID, WindowID) ->
    lager:debug("Starting persistence worker ~p", [self()]),
    proc_lib:init_ack(Parent, {ok, self()}),
    case get_window(MetricID, WindowID) of
        not_found ->
            not_found;
        {ok, {WindowMeta, WindowData}} ->
            Chunks = apod:chunkify(WindowData),
            ok = persist_chunks(WindowMeta, Chunks)
    end.


get_window(MetricID, WindowID) ->
    case osc_cache:find(MetricID) of
        not_found ->
            not_found;
        {ok, Pid} ->
            osc_cache:get_window(Pid, WindowID)
    end.


persist_chunks(_, []) ->
    ok;
persist_chunks(WindowMeta, [{T, Chunk, Count, Size}|Chunks]) ->
    C = osc_persistence_util:commutator(),
    WindowID = osc_meta_window:id(WindowMeta),
    MetricID = osc_meta_window:metric_id(WindowMeta),
    {ok, true} = commutator:put_item(C, [WindowID, T, Chunk]),
    insert_persist(WindowMeta, T, Count),
    mstat:increment_counter([osc_persistence, persisted_chunks]),
    mstat:increment_counter([osc_persistence, persisted_points], Count),
    mstat:update_histogram([osc_persistence, chunk_size], Count),
    lager:debug("Persist attempt successful for window ~p", [WindowID]),
    osc_event:notify({persist, MetricID, WindowID, T, Count, Size}),
    persist_chunks(WindowMeta, Chunks).


insert_persist(WindowMeta, Timestamp, Count) ->
    try
        ok = osc_meta_window:insert_persist(WindowMeta, Timestamp, Count)
    catch
        error:{badmatch, {error, unique_violation}} ->
            %% This was previously successfully inserted -
            %% probably on a query that timed-out - so
            %% return
            ok;
        error:{badmatch, B} ->
            lager:warning("badmatch in persist insertion: ~p", [B]),
            timer:sleep(trunc(1000 * random:uniform())),
            insert_persist(WindowMeta, Timestamp, Count)
    end.


