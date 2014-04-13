-module(oscilloscope_metadata_metrics).

-export([
    create/3,
    get/1,
    insert_persisted/3,
    delete_persisted/2,
    get_aggregation_configuration/1,
    get_resolution_configuration/1
]).

-include("oscilloscope_metadata.hrl").

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

-spec create(metric_key(), aggregation(), [resolution()]) -> ok.
create(MetricKey, AggregationFun, Resolutions) ->
    gen_server:call(
        oscilloscope_metadata_manager,
        [{create, MetricKey, AggregationFun, Resolutions}]
    ).

-spec get(metric_key()) ->
  {ok, {aggregation(), [resolution()]}} | not_found.
get(MetricKey) ->
    gen_server:call(oscilloscope_metadata_manager, [{get, MetricKey}]).

-spec get_metric_aggregation(metric_key()) ->
  {ok, aggregation()} | not_found.
get_metric_aggregation(MetricKey) ->
    gen_server:call(
        oscilloscope_metadata_manager,
        [{get_metric_aggregation, MetricKey}]
    ).

-spec get_metric_resolutions(metric_key()) ->
  {ok, [resolution()]}.
get_metric_resolutions(MetricKey) ->
    gen_server:call(
        oscilloscope_metadata_manager,
        [{get_metric_resolutions, MetricKey}]
    ).

-spec get_metric_persists(resolution_id()) ->
  {ok, [{timestamp(), pos_integer()}]}.
get_metric_persists(ResolutionID) ->
    gen_server:call(
        oscilloscope_metadata_manager,
        [{get_metric_persists, ResolutionID}]
    ).

-spec insert_persisted(resolution_id(), timestamp(), pos_integer()) -> ok.
insert_persisted(Id, PersistTime, Count) ->
    gen_server:call(
        oscilloscope_metadata_manager,
        [{insert_persisted, Id, PersistTime, Count}]
    ).

delete_persisted(Id, PersistTime) ->
    gen_server:call(
        oscilloscope_metadata_manager,
        [{delete_persisted, Id, PersistTime}]
    ).

-spec get_aggregation_configuration() -> {ok, [resolution()]}.
get_aggregation_configuration() ->
    gen_server:call(oscilloscope_metadata_manager, []).

-spec get_resolution_configuration() -> {ok, [resolution()]}.
get_resolution_configuration() ->
    gen_server:call(oscilloscope_metadata_manager, []).
