-module(osc_meta_metric).

-export([
    create/1,
    lookup/1,
    search/1,
    windows/1,
    name/1
]).

-include_lib("osc/include/osc_types.hrl").
-include_lib("osc_meta/include/osc_meta.hrl").

-spec create(Metric) -> {ok, MetricID} | {error, Error} when
    Metric :: metric(),
    MetricID :: metric_id(),
    Error :: exists.

create({OwnerID, Props}=Metric) ->
    EncodedProps = term_to_binary(lists:sort(Props)),
    GetMetricSQL = <<
        "SELECT id FROM metrics"
        " WHERE owner_id = $1 AND hash = $2;"
    >>,
    {ok, _, Rows} = osc_sql:adhoc(
        GetMetricSQL, [OwnerID, EncodedProps]
    ),
    case Rows of
        [] ->
            {ok, 1, _, [{MetricID}]} = osc_sql:named(
                add_metric, [OwnerID, EncodedProps]
            ),
            lists:map(fun({Key, Value}) ->
                {ok, 1} = osc_sql:named(add_tag, [MetricID, Key, Value])
            end, Props),
            {ok, Windows} = osc_meta_window_configuration:for_metric(Metric),
            lists:foreach(
                fun(W) -> osc_meta_window:create(MetricID, W) end,
                Windows
            ),
            {ok, MetricID};
        [{MetricID}] ->
            {error, {exists, MetricID}}
    end.


-spec lookup(Metric) -> {ok, Meta} | not_found when
    Metric :: metric_id() | metric(),
    Meta :: metricmeta().

lookup(Metric) ->
    {ok, _, Info} = case Metric of
        {OwnerID0, MetricName} ->
            LookupSQL = "SELECT id, owner_id, hash FROM metrics "
                        "WHERE owner_id = $1 AND hash = $2",
            osc_sql:adhoc(LookupSQL, [OwnerID0, MetricName]);
        Metric ->
            LookupSQL = "SELECT id, owner_id, hash FROM metrics WHERE id = $1",
            osc_sql:adhoc(LookupSQL, [Metric])
    end,
    case Info of
        [] ->
            not_found;
        [{MetricID, OwnerID, EncodedProps}] ->
            PropSQL = "SELECT key, value FROM tags WHERE metric_id = $1",
            {ok, _, Props} = osc_sql:adhoc(PropSQL, [MetricID]),
            Windows = osc_meta_window:lookup(MetricID),
            {ok, #metricmeta{
                id = MetricID,
                owner_id = OwnerID,
                props = Props,
                encoded_props = EncodedProps,
                windows = Windows
            }}
    end.


-spec search(metric()) -> [].
search({OwnerID, [{Key, Value}]}) ->
    % Metrics with only one key-value pair are supported right now
    SQL = "SELECT id FROM metrics "
          "JOIN tags ON metrics.id = tags.metric_id "
          "WHERE metrics.owner_id=$1 "
          "AND tags.key=$2 "
          "AND tags.value~$3;",
    {ok, _, Resp} = osc_sql:adhoc(SQL, [OwnerID, Key, Value]),
    [MetricID || {MetricID} <- Resp].


windows(#metricmeta{windows=Windows}) ->
    Windows.


name(#metricmeta{owner_id=OwnerID, encoded_props=EncodedProps}) ->
    {OwnerID, EncodedProps}.
