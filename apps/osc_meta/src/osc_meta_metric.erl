-module(osc_meta_metric).

-export([create/1, lookup/1]).

-include_lib("osc/include/osc_types.hrl").
-include("osc_meta.hrl").

-spec create({owner_id(), [{binary(), binary()}]}) -> metric_id().
create({OwnerID, Props}=Metric) ->
    EncodedProps = term_to_binary({OwnerID, lists:sort(Props)}),
    {ok, AggFun} = get_aggregation_configuration(Metric),
    {ok, Resolutions} = get_resolution_configuration(Metric),
    osc_sql:transaction(fun(C, Smts) ->
        CallSQL = fun(Smt, Args) ->
            SQL = proplists:get_value(Smt, Smts),
            pgsql:equery(C, SQL, Args)
        end,

        {ok, 1, _, [{MetricID}]} = CallSQL(
            insert_metric,
            [OwnerID, EncodedProps, term_to_binary(AggFun)]
        ),
        lists:map(fun({Interval, Count}) ->
            {ok, 1} = CallSQL(insert_resolution, [MetricID, Interval, Count])
        end, Resolutions),
        lists:map(fun({Key, Value}) ->
            {ok, 1} = CallSQL(insert_tag, [MetricID, Key, Value])
        end, Props),
        MetricID
    end),
    lookup(Metric).

lookup({OwnerID, Props}) ->
    EncodedProps = term_to_binary(lists:sort(Props)),
    {ok, _, Rows} = osc_sql:named(
        select_metric, [OwnerID, EncodedProps]
    ),
    case Rows of
        [{MetricID, AggBin, _, _, _}|_] ->
            Resolutions = lists:foldl(
                fun({_, _, ResolutionID, Interval, Count}, Acc) ->
                    {ok, _, Persists} = osc_sql:named(
                        select_persists, [ResolutionID]
                    ),
                    [{ResolutionID, Interval, Count, Persists}|Acc]
                end,
                [],
                Rows
            ),
            Meta = [
                {owner, OwnerID},
                {props, Props},
                {encoded_props, EncodedProps},
                {id, MetricID},
                {aggregation, binary_to_term(AggBin)},
                {resolutions, Resolutions}
            ],
            {ok, Meta};
        [] ->
            not_found
    end.

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
