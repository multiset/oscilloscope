-module(osc_meta_metric).

-export([
    create/1,
    lookup/1,
    search/1,
    windows/1
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
                {ok, 1} = osc_sql:named(add_tag, [OwnerID, MetricID, Key, Value])
            end, Props),
            {ok, Windows} = osc_meta_window_configuration:for_metric(Metric),
            lists:foreach(
                fun(W) -> osc_meta_window:create(MetricID, W) end,
                Windows
            ),
            {ok, MetricID};
        _ ->
            {error, exists}
    end.


-spec lookup(MetricID) -> {ok, Meta} | not_found when
    MetricID :: metric_id(),
    Meta :: metricmeta().

lookup(MetricID) ->
    LookupSQL = "SELECT owner_id, hash FROM metrics WHERE id = $1",
    {ok, _, ID} = osc_sql:adhoc(LookupSQL, [MetricID]),
    case ID of
        [] ->
            not_found;
        [{OwnerID, EncodedProps}] ->
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
    {ok, _, Resp} = osc_sql:adhoc(
        <<"SELECT metric_id FROM tags WHERE owner_id=$1 AND key=$2 AND value~$3">>,
        [OwnerID, Key, Value]
    ),
    [MetricID || {MetricID} <- Resp].


windows(#metricmeta{windows=Windows}) ->
    Windows.
