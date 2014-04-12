-module(oscilloscope_sql_metrics).

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
    Metric = #metric{
        key=MetricKey,
        aggregation_fun=term_to_binary(AggregationFun)
    },
    ok = ets:insert(metrics, Metric),
    lists:foreach(
        fun({Interval, Count}) ->
            Resolution = #resolution{
                key=MetricKey,
                interval=Interval,
                count=Count
            },
            ok = ets:insert(resolutions, Resolution)
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
    Match = #metric{key=MetricKey, aggregation='$1'},
    case ets:match_object(metrics, Match) of
        [Aggregation] ->
            {ok, Aggregation};

        [] ->
            not_found
    end.

-spec get_metric_resolutions(metric_key()) ->
  {ok, [resolution()]}.
get_metric_resolutions(MetricKey) ->
    Match = #resolution{key=MetricKey, interval='$1', count='$2'},
    case ets:match_object(resolutions, Match) of
        [] ->
            not_found;
        Resolutions ->
            {ok, Resolutions}
    end.

-spec get_metric_persists(resolution_id()) ->
  {ok, [{timestamp(), pos_integer()}]}.
get_metric_persists(ResolutionID) ->
    Match = #persist{key=ResolutionID, timestamp='$1', count='$2'},
    {ok, ets:match_object(persists, Match)}.

-spec insert_persisted(resolution_id(), timestamp(), pos_integer()) -> ok.
insert_persisted(Id, PersistTime, Count) ->
    Persist = #persist{key=Id, timestamp=PersistTime, count=Count},
    ets:insert(persists, Persist).

delete_persisted(Id, PersistTime) ->
    ets:delet_object(persists, #persist{key=Id, timestamp=PersistTime}),
    ok.

-spec get_aggregation_configuration(userid()) -> {ok, [resolution()]}.
get_aggregation_configuration(_UserID) ->
    {ok, []}.

-spec get_resolution_configuration(userid()) -> {ok, [resolution()]}.
get_resolution_configuration(_UserID) ->
    {ok, []}.
