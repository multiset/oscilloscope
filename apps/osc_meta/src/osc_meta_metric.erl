-module(osc_meta_metric).

-export([
    create/1,
    lookup/1,
    search/1,
    windows/1
]).

-include_lib("osc/include/osc_types.hrl").

-record(metricmeta, {
    id :: metric_id(),
    owner_id :: owner_id(),
    props :: any(), %% TODO
    encoded_props :: binary(),
    windows :: [osc_meta_window:windowmeta()]
}).

-opaque metricmeta() :: #metricmeta{}.
-export_type([metricmeta/0]).

-spec create({owner_id(), meta()}) -> {ok, metricmeta()} | not_found.
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
            {ok, Windows} = get_window_configuration(Metric),
            lists:foreach(
                fun(W) -> osc_meta_window:create(MetricID, W) end,
                Windows
            );
        _ ->
            %% TODO: Log
            ok
    end,
    lookup(Metric).


-spec lookup({owner_id(), meta()}) -> {ok, metricmeta()} | not_found.
lookup({OwnerID, Props}) ->
    EncodedProps = term_to_binary(lists:sort(Props)),
    SQL = <<"SELECT id FROM metrics WHERE owner_id = $1 AND hash = $2">>,
    {ok, _, IDs} = osc_sql:adhoc(
        SQL, [OwnerID, EncodedProps]
    ),
    case IDs of
        [] ->
            not_found;
        [{MetricID}] ->
            Windows = osc_meta_window:lookup(MetricID),
            {ok, #metricmeta{
                id = MetricID,
                owner_id = OwnerID,
                props = Props,
                encoded_props = EncodedProps,
                windows = Windows
            }}
    end.


-spec search({owner_id(), meta()}) -> [].
search({OwnerID, [{Key, Value}]}) ->
    % Metrics with only one key-value pair are supported right now
    {ok, _, Resp} = osc_sql:adhoc(
        <<"SELECT metric_id FROM tags WHERE owner_id=$1 AND key=$2 AND value~$3">>,
        [OwnerID, Key, Value]
    ),
    [MetricID || {MetricID} <- Resp].


windows(#metricmeta{windows=Windows}) ->
    Windows.


get_window_configuration(_) ->
    application:get_env(
        osc_meta,
        default_window_configuration
    ).
