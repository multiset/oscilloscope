-module(osc_meta_metric).

-export([
    create/1,
    lookup/1,
    name/1,
    search/2,
    id/1,
    windows/1,
    props/1
]).

-include_lib("osc/include/osc_types.hrl").
-include_lib("osc_meta/include/osc_meta.hrl").

-spec create(Metric) -> {ok, MetricID} | {error, Error} when
    Metric :: metric(),
    MetricID :: metric_id(),
    Error :: exists | missing_owner.

create({OrgID, Props}=Metric) ->
    EncodedProps = term_to_binary(lists:sort(Props)),
    ok = mpgsql:tx_begin(),
    InsertSQL = "INSERT INTO metrics (org_id, hash) "
                "VALUES ($1, $2) RETURNING id;",
    case mpgsql:equery(InsertSQL, [OrgID, EncodedProps]) of
        {error, unique_violation} ->
            ok = mpgsql:tx_rollback(),
            {error, exists};
        {error, foreign_key_violation} ->
            ok = mpgsql:tx_rollback(),
            {error, missing_owner};
        {ok, 1, _, [{MetricID}]} ->
            {ok, Windows} =  osc_meta_window_configuration:for_metric(Metric),
            InsertWindowSQL = "INSERT INTO windows "
                              "(metric_id, type, aggregation, "
                              "interval, count) VALUES "
                              "($1, $2, $3, $4, $5);",
            lists:foreach(
                fun({Type, Aggregation, Interval, Count}) ->
                    Args = [
                        MetricID,
                        term_to_binary(Type),
                        term_to_binary(Aggregation),
                        Interval,
                        Count
                    ],
                    {ok, 1} = mpgsql:equery(InsertWindowSQL, Args)
                end,
                Windows
            ),
            InsertTagSQL = "INSERT INTO TAGS "
                           "(metric_id, key, value) "
                           "VALUES ($1, $2, $3);",

            lists:foreach(
                fun({Key, Value}) ->
                    {ok, 1} = mpgsql:equery(
                        InsertTagSQL,
                        [MetricID, Key, Value]
                    )
                end,
                Props
            ),
            ok = mpgsql:tx_commit(),
            mstat:increment_counter(
                [osc_meta, creations, window],
                length(Windows)
            ),
            mstat:increment_counter([osc_meta, creations, metric]),
            {ok, MetricID}
    end.

-spec lookup(Metric) -> {ok, Meta} | not_found when
    Metric :: metric_id() | metric(),
    Meta :: metricmeta().

lookup(Metric) ->
    {ok, _, Info} = case Metric of
        {OrgID0, Props0} ->
            %% Use Props0 here because the value from the SQL below may be in a
            %% different order - the ordering in the database is undefined.
            EncodedProps0 = term_to_binary(lists:sort(Props0)),
            LookupSQL = "SELECT id, org_id, hash FROM metrics "
                        "WHERE org_id = $1 AND hash = $2",
            mpgsql:equery(LookupSQL, [OrgID0, EncodedProps0]);
        _ ->
            LookupSQL = "SELECT id, org_id, hash FROM metrics WHERE id = $1",
            mpgsql:equery(LookupSQL, [Metric])
    end,
    case Info of
        [] ->
            not_found;
        [{MetricID, OrgID, EncodedProps}] ->
            PropSQL = "SELECT key, value FROM tags WHERE metric_id = $1",
            {ok, _, Props1} = mpgsql:equery(PropSQL, [MetricID]),
            Windows = osc_meta_window:lookup(MetricID),
            {ok, #metricmeta{
                id = MetricID,
                org_id = OrgID,
                props = Props1,
                encoded_props = EncodedProps,
                windows = Windows
            }}
    end.

-spec search(org_id(), [{group_tag_key(), group_tag_value()}]) -> [].
search(OrgID, [{Key, Value}]) ->
    % Metrics with only one key-value pair are supported right now
    SQL = "SELECT id FROM metrics "
          "JOIN tags ON metrics.id = tags.metric_id "
          "WHERE metrics.org_id = $1 "
          "AND tags.key = $2 "
          "AND tags.value ~ $3;",
    {ok, _, Resp} = mpgsql:equery(SQL, [OrgID, Key, Value]),
    [MetricID || {MetricID} <- Resp].

id(#metricmeta{id=ID}) ->
    ID.

windows(#metricmeta{windows=Windows}) ->
    Windows.

name(#metricmeta{org_id=OrgID, encoded_props=EncodedProps}) ->
    {OrgID, EncodedProps}.

props(#metricmeta{props=Props}) ->
    Props.
