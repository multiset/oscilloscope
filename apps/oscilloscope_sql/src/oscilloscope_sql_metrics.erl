-module(oscilloscope_sql_metrics).

-export([
    create/5,
    get/3,
    update_persisted/2
]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

-spec create(user(), service(), host(), aggregation(), [resolution()]) -> ok.
create(User, Service, Host, AggregationFun, Resolutions) ->
    {ok, 1} = oscilloscope_sql:named(
        insert_metric, [User, Service, Host, term_to_binary(AggregationFun)]
    ),
    lists:foreach(
        fun({Interval, Count, Persisted}) ->
            {ok, 1} = oscilloscope_sql:named(
                insert_resolution,
                [User, Service, Host, Interval, Count, Persisted]
            )
        end,
        Resolutions
    ).

-spec get(user(), service(), host()) ->
  {ok, {aggregation(), [resolution()]}} | {error, not_found}.
get(User, Service, Host) ->
    {ok, _AggSchema, AggRows} = oscilloscope_sql:named(
        select_metric_aggregation, [User, Service, Host]
    ),
    case AggRows of
        [{AggBin}] ->
            {ok, _Schema, Resolutions} = oscilloscope_sql:named(
                select_metric_resolutions, [User, Service, Host]
            ),
            {ok, {binary_to_term(AggBin), Resolutions}};
        [] ->
            {error, not_found}
    end.

-spec update_persisted(resolution_id(), timestamp()) -> ok.
update_persisted(Id, PersistTime) ->
    {ok, _Count} = oscilloscope_sql:named(
        update_persisted, [Id, PersistTime]
    ),
    ok.
