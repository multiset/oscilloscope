-module(oscilloscope_sql_metrics).

-export([
    create/5,
    get/3,
    update_persisted/2
]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

-spec create(userid(), service(), host(), aggregation(), [resolution()]) -> ok.
create(UserID, Service, Host, AggregationFun, Resolutions) ->
    {ok, 1} = oscilloscope_sql:named(
        insert_metric, [UserID, Service, Host, term_to_binary(AggregationFun)]
    ),
    lists:foreach(
        fun({Interval, Count, Persisted}) ->
            {ok, 1} = oscilloscope_sql:named(
                insert_resolution,
                [UserID, Service, Host, Interval, Count, Persisted]
            )
        end,
        Resolutions
    ).

-spec get(userid(), service(), host()) ->
  {ok, {aggregation(), [resolution()]}} | {error, not_found}.
get(UserID, Service, Host) ->
    {ok, _AggSchema, AggRows} = oscilloscope_sql:named(
        select_metric_aggregation, [UserID, Service, Host]
    ),
    case AggRows of
        [{AggBin}] ->
            {ok, _Schema, Resolutions} = oscilloscope_sql:named(
                select_metric_resolutions, [UserID, Service, Host]
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
