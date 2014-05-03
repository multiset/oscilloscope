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

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, ok}.


handle_call({create, MetricKey, AggregationFun, Resolutions}, _From, _State) ->
    {ok, 1} = oscilloscope_sql:named(
        insert_metric, [MetricKey, term_to_binary(AggregationFun)]
    ),
    lists:foreach(
        fun({Interval, Count}) ->
            {ok, 1} = oscilloscope_sql:named(
                insert_resolution,
                [MetricKey, Interval, Count]
            )
        end,
        Resolutions
    ),
    {ok, ok};

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

handle_call({insert_persisted, Id, PersistTime, Count}, _From, _State) ->
    {ok, _Count} = oscilloscope_sql:named(
        insert_persist, [Id, PersistTime, Count]
    ),
    ok;

handle_call({delete_persisted, Id, PersistTime}, _From, _State) ->
    {ok, _Count} = oscilloscope_sql:named(
        delete_persist, [Id, PersistTime]
    ),
    ok;

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
    {ok, _AggSchema, AggRows} = oscilloscope_sql:named(
        select_metric_aggregation, [MetricKey]
    ),
    case AggRows of
        [{AggBin}] ->
            {ok, binary_to_term(AggBin)};
        [] ->
            not_found
    end.

get_metric_resolutions(MetricKey) ->
    {ok, _Schema, Resolutions} = oscilloscope_sql:named(
        select_metric_resolutions, [MetricKey]
    ),
    {ok, Resolutions}.


get_metric_persists(ResolutionID) ->
    {ok, _Schema, Persists} = oscilloscope_sql:named(
        select_metric_persists, [ResolutionID]
    ),
    {ok, Persists}.
