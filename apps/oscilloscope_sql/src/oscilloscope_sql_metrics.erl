-module(oscilloscope_sql_metrics).

-export([
    create/5,
    get/3,
    insert_persisted/3,
    get_aggregation_configuration/1,
    get_resolution_configuration/1
]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

-spec create(userid(), service(), host(), aggregation(), [resolution()]) -> ok.
create(UserID, Service, Host, AggregationFun, Resolutions) ->
    {ok, 1} = oscilloscope_sql:named(
        insert_metric, [UserID, Service, Host, term_to_binary(AggregationFun)]
    ),
    lists:foreach(
        fun({Interval, Count}) ->
            {ok, 1} = oscilloscope_sql:named(
                insert_resolution,
                [UserID, Service, Host, Interval, Count]
            )
        end,
        Resolutions
    ).

-spec get(userid(), service(), host()) ->
  {ok, {aggregation(), [resolution()]}} | not_found.
get(UserID, Service, Host) ->
    case get_metric_aggregation(UserID, Service, Host) of
        not_found ->
            not_found;
        {ok, Aggregation} ->
            {ok, Resolutions} = get_metric_resolutions(UserID, Service, Host),
            Resolutions1 = lists:map(
                fun({ResID, Interval, Count}) ->
                    {ok, Persisted} = get_metric_persists(ResID),
                    {ResID, Interval, Count, Persisted} end,
                Resolutions
            ),
            {ok, {Aggregation, Resolutions1}}
    end.

-spec get_metric_aggregation(userid(), service(), host()) ->
  {ok, aggregation()} | not_found.
get_metric_aggregation(UserID, Service, Host) ->
    {ok, _AggSchema, AggRows} = oscilloscope_sql:named(
        select_metric_aggregation, [UserID, Service, Host]
    ),
    case AggRows of
        [{AggBin}] ->
            {ok, binary_to_term(AggBin)};

        [] ->
            not_found
    end.

-spec get_metric_resolutions(userid(), service(), host()) ->
  {ok, [resolution()]}.
get_metric_resolutions(UserID, Service, Host) ->
    {ok, _Schema, Resolutions} = oscilloscope_sql:named(
        select_metric_resolutions, [UserID, Service, Host]
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

-spec get_aggregation_configuration(userid()) -> {ok, [resolution()]}.
get_aggregation_configuration(_UserID) ->
    {ok, []}.

-spec get_resolution_configuration(userid()) -> {ok, [resolution()]}.
get_resolution_configuration(_UserID) ->
    {ok, []}.
