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
    {ok, #state{}}.


handle_call({create, MetricKey, AggregationFun, Resolutions}, _From, State) ->
    Metric = #metric{
        key=MetricKey,
        aggregation_fun=term_to_binary(AggregationFun)
    },
    ok = ets:insert(metrics, Metric),
    Reply = lists:foreach(
        fun({Interval, Count}) ->
            Resolution = #resolution{
                key=MetricKey,
                interval=Interval,
                count=Count
            },
            ok = ets:insert(resolutions, Resolution)
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
    Match = #metric{key=MetricKey, aggregation='$1'},
    Reply = case ets:match_object(metrics, Match) of
        [Aggregation] ->
            {ok, Aggregation};

        [] ->
            not_found
    end,
    {ok, Reply};

handle_call({get_metric_resolutions, MetricKey}, _From, _State) ->
    Match = #resolution{key=MetricKey, interval='$1', count='$2'},
    Reply = case ets:match_object(resolutions, Match) of
        [] ->
            not_found;
        Resolutions ->
            {ok, Resolutions}
    end,
    {ok, Reply};

handle_call({get_metric_persists, ResolutionID}, _From, _State) ->
    Match = #persist{key=ResolutionID, timestamp='$1', count='$2'},
    {ok, ets:match_object(persists, Match)};

handle_call({insert_persisted, Id, PersistTime, Count}, _From, _State) ->
    Persist = #persist{key=Id, timestamp=PersistTime, count=Count},
    {ok, ets:insert(persists, Persist)};

handle_call({delete_persisted, Id, PersistTime}, _From, _State) ->
    ets:delet_object(persists, #persist{key=Id, timestamp=PersistTime}),
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
