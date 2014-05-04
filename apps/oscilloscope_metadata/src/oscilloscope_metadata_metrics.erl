-module(oscilloscope_metadata_metrics).

-export([
    create/3,
    get/1,
    insert_persisted/3,
    delete_persisted/2,
    get_aggregation_configuration/0,
    get_resolution_configuration/0
]).

-include("oscilloscope_metadata.hrl").

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

-spec create(metric(), aggregation(), [{interval(), count()}]) -> ok.
create({OwnerId, [{<<"graphite">>, Name}]}, AggregationFun, Resolutions) ->
    %% TODO: Transaction this
    {ok, 1} = oscilloscope_metadata:named(
        insert_metric, [OwnerId, Name, term_to_binary(AggregationFun)]
    ),
    lists:foreach(
        fun({Interval, Count}) ->
            {ok, 1} = oscilloscope_metadata:named(
                insert_resolution,
                [OwnerId, Name, Interval, Count]
            )
        end,
        Resolutions
    ).

-spec get(metric()) ->
  {ok, {metric_id(), aggregation(), [resolution()]}} | not_found.
get({OwnerId, [{<<"graphite">>, Name}]}) ->
    {ok, _, Rows} = oscilloscope_metadata:named(
        select_metric, [OwnerId, Name]
    ),
    case Rows of
        [] ->
            not_found;
        [{MetricId, AggBin, _, _, _}|_] ->
            Resolutions = lists:foldl(
                fun({_, _, ResolutionId, Interval, Count}, Acc) ->
                    {ok, _, Persists} = oscilloscope_metadata:named(
                        select_resolution_persists, [ResolutionId]
                    ),
                    [{ResolutionId, Interval, Count, Persists}|Acc]
                end,
                [],
                Rows
            ),
            {ok, {MetricId, binary_to_term(AggBin), Resolutions}}
    end.

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
