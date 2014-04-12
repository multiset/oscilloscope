-module(oscilloscope_metadata_manager).

-behaviour(gen_server).

-export([start_link/0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("oscilloscope_metadata.hrl").

-record(state, {
    metric_db,
    resolution_db,
    persist_db
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    ets:new(metrics, [{keypos, #metric.key}, named_table]),
    ets:new(resolutions, [{keypos, #resolution.key, named_table, bag}]),
    ets:new(persists, [{keypos, #persist.key}, named_table, bag]),
    MetricsDbPath = application:get_env(oscilloscope_metadata, metrics_db),
    ResoDbPath = application:get_env(oscilloscope_metadata, resolutions_db),
    PersistDbPath = application:get_env(oscilloscope_metadata, persists_db),
    MetricsDb = eleveldb:open(MetricsDbPath, [{create_if_missing, true}]),
    ResoDb = eleveldb:open(ResoDbPath, [{create_if_missing, true}]),
    PersistDb = eleveldb:open(PersistDbPath, [{create_if_missing, true}]),
    eleveldb:fold(MetricsDb, fun(Key, Value, _Acc) ->
        Metric = #metric{
            key=binary_to_term(Key),
            aggregation=binary_to_term(Value)
        },
        ets:insert(metrics, Metric)
    end, ok, []),
    eleveldb:fold(ResoDb, fun(Key, Value, _Acc) ->
        {Interval, Count} = binary_to_term(Value),
        Reso = #resolution{
            key=binary_to_term(Key),
            interval=Interval,
            count=Count
        },
        ets:insert(resolutions, Reso)
    end, ok, []),
    eleveldb:fold(PersistDb, fun(Key, Value, _Acc) ->
        {Timestamp, Count} = binary_to_term(Value),
        Persist = #persist{
            key=binary_to_term(Key),
            timestamp=Timestamp,
            count=Count
        },
        ets:insert(persists, Persist)
    end, ok, []),
    State = #state{
        metric_db=MetricsDb,
        resolution_db=ResoDb,
        persist_db=PersistDb
    },
    {ok, State}.


handle_call({create, MetricKey, AggregationFun, Resolutions}, _From, State) ->
    AggFunBin = term_to_binary(AggregationFun),
    #state{metric_db=MetricDb, resolution_db=ResoDb} = State,
    Metric = #metric{
        key=MetricKey,
        aggregation=AggFunBin
    },
    ok = ets:insert(metrics, Metric),
    MetricKeyBin = term_to_binary(MetricKey),
    ok = eleveldb:put(MetricDb, MetricKeyBin, AggFunBin, []),
    Reply = lists:foreach(
        fun({Interval, Count}) ->
            Resolution = #resolution{
                key=MetricKeyBin,
                interval=Interval,
                count=Count
            },
            ResoBin = term_to_binary({Interval, Count}),
            ok = ets:insert(resolutions, Resolution),
            ok = eleveldb:put(ResoDb, MetricKeyBin, ResoBin, [])
        end,
        Resolutions
    ),
    {ok, Reply};

handle_call({get, MetricKey}, _From, _State) ->
    Reply = case get_metric_aggregation(MetricKey) of
        not_found ->
            not_found;
        {ok, Aggregation} ->
            {ok, Resolutions} = get_metric_resolutions(MetricKey),
            Resolutions1 = lists:map(
                fun({ResID, Interval, Count}) ->
                    {ok, Persisted} = get_metric_persists(ResID),
                    {ResID, Interval, Count, Persisted} end,
                Resolutions
            ),
            {ok, {Aggregation, Resolutions1}}
    end,
    {ok, Reply};

handle_call({get_metric_aggregation, MetricKey}, _From, _State) ->
    {ok, get_metric_aggregation(MetricKey)};

handle_call({get_metric_resolutions, MetricKey}, _From, _State) ->
    {ok, get_metric_resolutions(MetricKey)};

handle_call({get_metric_persists, ResolutionID}, _From, _State) ->
    {ok, get_metric_persists(ResolutionID)};

handle_call({insert_persisted, Id, PersistTime, Count}, _From, State) ->
    Persist = #persist{key=Id, timestamp=PersistTime, count=Count},
    IdBin = term_to_binary({Id, PersistTime}),
    ValBin = term_to_binary(Count),
    eleveldb:put(State#state.persist_db, IdBin, ValBin, []),
    {ok, ets:insert(persists, Persist)};

handle_call({delete_persisted, Id, PersistTime}, _From, State) ->
    #state{persist_db=PersistDb} = State,
    ok = eleveldb:delete(PersistDb, term_to_binary({Id, PersistTime}), []),
    ets:delete_object(persists, #persist{key=Id, timestamp=PersistTime}),
    {ok, ok};

handle_call({get_aggregation_configuration}, _From, _State) ->
    {ok, []};

handle_call({get_resolution_configuration}, _From, _State) ->
    {ok, []}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


get_metric_aggregation(MetricKey) ->
    Match = #metric{key=MetricKey, aggregation='$1'},
    case ets:match_object(metrics, Match) of
        [Aggregation] ->
            {ok, Aggregation};

        [] ->
            not_found
    end.


get_metric_resolutions(MetricKey) ->
    Match = #resolution{key=MetricKey, interval='$1', count='$2'},
    case ets:match_object(resolutions, Match) of
        [] ->
            not_found;
        Resolutions ->
            {ok, Resolutions}
    end.


get_metric_persists(ResolutionID) ->
    Match = #persist{key=ResolutionID, timestamp='$1', count='$2'},
    ets:match_object(persists, Match).
