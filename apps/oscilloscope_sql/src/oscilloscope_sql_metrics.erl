-module(oscilloscope_sql_metrics).

-export([
    create/5,
    get/3,
    update_persisted/6
]).

-type resolution() :: {pos_integer(), pos_integer(), [pos_integer()]}.

-spec create(binary(), binary(), binary(), atom(), [resolution()]) -> ok.
create(User, Name, Host, AggregationFun, Resolutions) ->
    {ok, 1} = oscilloscope_sql:named(
        insert_metric, [User, Name, Host, term_to_binary(AggregationFun)]
    ),
    lists:foreach(
        fun({Interval, Count, Persisted}) ->
            {ok, 1} = oscilloscope_sql:named(
                insert_resolution,
                [User, Name, Host, Interval, Count, Persisted]
            )
        end,
        Resolutions
    ).

-spec get(binary(), binary(), binary()) ->
  {ok, {atom(), [resolution()]}} | {error, not_found}.
get(User, Name, Host) ->
    io:format("Getting for ~p ~p ~p~n", [User, Name, Host]),
    {ok, _AggSchema, AggRows} = oscilloscope_sql:named(
        select_metric_aggregation, [User, Name, Host]
    ),
    case AggRows of
        [{AggBin}] ->
            {ok, _Schema, Rows} = oscilloscope_sql:named(
                select_metric_resolutions, [User, Name, Host]
            ),
            Resolutions = lists:map(
                fun({Id, _, Interval, Count, Persisted}) ->
                    {Id, Interval, Count, Persisted}
                end,
            Rows
            ),
            {ok, {binary_to_term(AggBin), Resolutions}};
        [] ->
            {error, not_found}
    end.

update_persisted(User, Name, Host, Interval, Count, PersistTime) ->
    {ok, _Count} = oscilloscope_sql:named(
        update_persisted, [User, Name, Host, Interval, Count, PersistTime]
    ),
    ok.
