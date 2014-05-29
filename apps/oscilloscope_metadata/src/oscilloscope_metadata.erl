-module(oscilloscope_metadata).

-export([
    start/0,
    stop/0
]).

-export([
    create/1,
    find/1,
    aggregation/1,
    resolutions/1
]).

-export_type([meta/0]).

-type meta() :: [{atom(), any()}].

start() ->
    application:start(oscilloscope_metadata).

stop() ->
    application:stop(oscilloscope_metadata).

%% TODO: consider storing this all in ETS and returning a pointer/ID

create({OwnerID, Props}=Metric) ->
    EncodedProps = term_to_binary(lists:sort(Props)),
    {ok, _, Rows} = oscilloscope_metadata_sql:named(
        select_metric, [OwnerID, EncodedProps]
    ),
    case Rows of
        [] ->
            {ok, AggFun} = get_aggregation_configuration(Metric),
            {ok, Resolutions} = get_resolution_configuration(Metric),
            {ok, 1} = oscilloscope_metadata_sql:named(
                insert_metric, [OwnerID, EncodedProps, term_to_binary(AggFun)]
            ),
            {ok, _, [{MetricID}]} = oscilloscope_metadata_sql:named(
                select_metric_id, [OwnerID, EncodedProps]
            ),
            lists:foreach(
                fun({Interval, Count}) ->
                    {ok, 1} = oscilloscope_metadata_sql:named(
                        insert_resolution,
                        [MetricID, Interval, Count]
                    )
                end,
                Resolutions
            );
        _ ->
            %% TODO: Log
            ok
    end,
    find(Metric).

find({OwnerID, Props}) ->
    EncodedProps = term_to_binary(lists:sort(Props)),
    {ok, _, Rows} = oscilloscope_metadata_sql:named(
        select_metric, [OwnerID, EncodedProps]
    ),
    case Rows of
        [] ->
            {error, not_found};
        [{MetricID, AggBin, _, _, _}|_] ->
            Resolutions = lists:foldl(
                fun({_, _, ResolutionID, Interval, Count}, Acc) ->
                    {ok, _, Persists} = oscilloscope_metadata_sql:named(
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
            {ok, Meta}
    end.

aggregation(Meta) ->
    proplists:get_value(aggregation, Meta).

resolutions(Meta) ->
    proplists:get_value(resolutions, Meta).

get_aggregation_configuration(_) ->
    %% TODO
    application:get_env(
        oscilloscope_metadata,
        default_aggregation_fun
    ).

get_resolution_configuration(_) ->
    %% TODO
    application:get_env(
        oscilloscope_metadata,
        default_resolutions
    ).
