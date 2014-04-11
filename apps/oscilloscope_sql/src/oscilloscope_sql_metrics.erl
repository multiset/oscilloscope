-module(oscilloscope_sql_metrics).

-export([
    create/4,
    get/2,
    insert_persisted/3,
    delete_persisted/2,
    get_aggregation_configuration/0,
    get_resolution_configuration/0
]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

-spec create(service(), host(), aggregation(), [resolution()]) -> ok.
create(Service, Host, AggregationFun, Resolutions) ->
    {ok, 1} = oscilloscope_sql:named(
        insert_metric, [Service, Host, term_to_binary(AggregationFun)]
    ),
    lists:foreach(
        fun({Interval, Count}) ->
            {ok, 1} = oscilloscope_sql:named(
                insert_resolution,
                [Service, Host, Interval, Count]
            )
        end,
        Resolutions
    ).

-spec get(service(), host()) ->
  {ok, {aggregation(), [resolution()]}} | not_found.
get(Service, Host) ->
    case get_metric_aggregation(Service, Host) of
        not_found ->
            not_found;
        {ok, Aggregation} ->
            {ok, Resolutions} = get_metric_resolutions(Service, Host),
            Resolutions1 = lists:map(
                fun({ResID, Interval, Count}) ->
                    {ok, Persisted} = get_metric_persists(ResID),
                    {ResID, Interval, Count, Persisted}
                end,
                Resolutions
            ),
            {ok, {Aggregation, Resolutions1}}
    end.

-spec get_metric_aggregation(service(), host()) ->
  {ok, aggregation()} | not_found.
get_metric_aggregation(Service, Host) ->
    {ok, _AggSchema, AggRows} = oscilloscope_sql:named(
        select_metric_aggregation, [Service, Host]
    ),
    case AggRows of
        [{AggBin}] ->
            {ok, binary_to_term(AggBin)};

        [] ->
            not_found
    end.

-spec get_metric_resolutions(service(), host()) ->
  {ok, [resolution()]}.
get_metric_resolutions(Service, Host) ->
    {ok, _Schema, Resolutions} = oscilloscope_sql:named(
        select_metric_resolutions, [Service, Host]
    ),
    {ok, Resolutions}.


-spec get_metric_persists(resolution_id()) ->
  {ok, [{timestamp(), pos_integer()}]}.
get_metric_persists(ResolutionID) ->
    {ok, _Schema, Persists} = oscilloscope_sql:named(
        select_metric_persists, [ResolutionID]
    ),
    {ok, Persists}.

-spec insert_persisted(resolution_id(), timestamp(), pos_integer()) -> ok.
insert_persisted(Id, PersistTime, Count) ->
    {ok, _Count} = oscilloscope_sql:named(
        insert_persist, [Id, PersistTime, Count]
    ),
    ok.

delete_persisted(Id, PersistTime) ->
    {ok, _Count} = oscilloscope_sql:named(
        delete_persist, [Id, PersistTime]
    ),
    ok.

-spec get_aggregation_configuration() -> {ok, [resolution()]}.
get_aggregation_configuration() ->
    {ok, []}.

-spec get_resolution_configuration() -> {ok, [resolution()]}.
get_resolution_configuration() ->
    {ok, []}.
