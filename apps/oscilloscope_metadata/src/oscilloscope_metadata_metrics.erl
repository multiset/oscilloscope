-module(oscilloscope_metadata_metrics).

-export([
    create/3,
    get/1,
    insert_persisted/3,
    delete_persisted/2,
    get_metric_aggregation/1,
    get_metric_resolutions/1,
    get_metric_persists/1,
    get_aggregation_configuration/0,
    get_resolution_configuration/0
]).

-include("oscilloscope_metadata.hrl").

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

-spec create(metric_key(), aggregation(), [resolution()]) -> ok.
create(MetricKey, AggregationFun, Resolutions) ->
    {ok, 1} = oscilloscope_metadata:named(
        insert_metric, [MetricKey, term_to_binary(AggregationFun)]
    ),
    lists:foreach(
        fun({Interval, Count}) ->
            {ok, 1} = oscilloscope_metadata:named(
                insert_resolution,
                [MetricKey, Interval, Count]
            )
        end,
        Resolutions
    ).

-spec get(metric_key()) ->
  {ok, {aggregation(), [resolution()]}} | not_found.
get(MetricKey) ->
    case get_metric_aggregation(MetricKey) of
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
    end.

-spec get_metric_aggregation(metric_key()) ->
  {ok, aggregation()} | not_found.
get_metric_aggregation(MetricKey) ->
    {ok, _AggSchema, AggRows} = oscilloscope_metadata:named(
        select_metric_aggregation, [MetricKey]
    ),
    case AggRows of
        [{AggBin}] ->
            {ok, binary_to_term(AggBin)};
        [] ->
            not_found
    end.

-spec get_metric_resolutions(metric_key()) ->
  {ok, [resolution()]}.
get_metric_resolutions(MetricKey) ->
    {ok, _Schema, Resolutions} = oscilloscope_metadata:named(
        select_metric_resolutions, [MetricKey]
    ),
    {ok, Resolutions}.

-spec get_metric_persists(resolution_id()) ->
  {ok, [{timestamp(), pos_integer()}]}.
get_metric_persists(ResolutionID) ->
    {ok, _Schema, Persists} = oscilloscope_metadata:named(
        select_metric_persists, [ResolutionID]
    ),
    {ok, Persists}.

-spec insert_persisted(resolution_id(), timestamp(), pos_integer()) -> ok.
insert_persisted(Id, PersistTime, Count) ->
    {ok, _Count} = oscilloscope_metadata:named(
        insert_persist, [Id, PersistTime, Count]
    ),
    ok.

-spec delete_persisted(resolution_id(), timestamp()) -> ok.
delete_persisted(Id, PersistTime) ->
    {ok, _Count} = oscilloscope_metadata:named(
        delete_persist, [Id, PersistTime]
    ),
    ok.

-spec get_aggregation_configuration() -> {ok, [resolution()]}.
get_aggregation_configuration() ->
    {ok, []}.

-spec get_resolution_configuration() -> {ok, [resolution()]}.
get_resolution_configuration() ->
    {ok, []}.
