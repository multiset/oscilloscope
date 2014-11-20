-module(osc_meta_metric).

-export([create/1, lookup/1, search/1]).

-include_lib("osc/include/osc_types.hrl").
-include("osc_meta.hrl").

-spec create({owner_id(), meta()}) -> {ok, meta()} | not_found.
create({OwnerID, Props}=Metric) ->
    EncodedProps = term_to_binary(lists:sort(Props)),

    {ok, _, Rows} = osc_sql:named(
        get_metric, [OwnerID, EncodedProps]
    ),
    case Rows of
        [] ->
            {ok, AggFun} = get_aggregation_configuration(Metric),
            {ok, Resolutions} = get_resolution_configuration(Metric),
            {ok, 1, _, [{MetricID}]} = osc_sql:named(
                add_metric, [OwnerID, EncodedProps, term_to_binary(AggFun)]
            ),
            lists:map(fun({Key, Value}) ->
                {ok, 1} = osc_sql:named(add_tag, [OwnerID, MetricID, Key, Value])
            end, Props),
            lists:foreach(
                fun({Interval, Count}) ->
                    {ok, 1} = osc_sql:named(
                        add_resolution,
                        [MetricID, Interval, Count]
                    )
                end,
                Resolutions
            );
        _ ->
            %% TODO: Log
            ok
    end,
    lookup(Metric).

-spec lookup({owner_id(), meta()}) -> {ok, meta()} | not_found.
lookup({OwnerID, Props}) ->
    EncodedProps = term_to_binary(lists:sort(Props)),
    {ok, _, ResoInfo} = osc_sql:named(
        get_metric_resolutions, [OwnerID, EncodedProps]
    ),
    case ResoInfo of
        [] ->
            not_found;
        [{MetricID, AggBin, _, _, _}|_] ->
            Resolutions = lists:foldl(
                fun({_, _, ResolutionID, Interval, Count}, Acc) ->
                    {ok, _, Persists} = osc_sql:named(
                        get_persists, [ResolutionID]
                    ),
                    [{ResolutionID, Interval, Count, Persists}|Acc]
                end,
                [],
                ResoInfo
            ),
            Meta = [
                {owner, OwnerID},
                {props, Props},
                {encoded_props, EncodedProps},
                {id, MetricID},
                {aggregation, binary_to_term(AggBin)},
                {resolutions, Resolutions}
            ],
            {ok, Meta}
    end.

-spec search({owner_id(), meta()}) -> [].
search({OwnerID, [{Key, Value}]}) ->
    % Metrics with only one key-value pair are supported right now
    {ok, _, Resp} = osc_sql:adhoc(
        <<"SELECT metric_id FROM tags WHERE owner_id=$1 AND key=$2 AND value~$3">>,
        [OwnerID, Key, Value]
    ),
    [MetricID || {MetricID} <- Resp].

-spec aggregation(Meta) -> Aggregation when
    Meta :: meta(),
    Aggregation :: aggregation().

aggregation(Meta) ->
    proplists:get_value(aggregation, Meta).

resolutions(Meta) ->
    proplists:get_value(resolutions, Meta).

get_aggregation_configuration(_) ->
    %% TODO
    application:get_env(
        osc_meta,
        default_aggregation_fun
    ).

get_resolution_configuration(_) ->
    %% TODO
    application:get_env(
        osc_meta,
        default_resolutions
    ).
