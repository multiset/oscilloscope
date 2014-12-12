-module(osc_meta_window).

-export([
    create/2,
    lookup/1,
    refresh/1,
    id/1,
    aggregation/1,
    interval/1,
    count/1,
    persisted/1,
    earliest_persisted_time/1,
    latest_persisted_time/1,
    insert_persist/3,
    delete_persist/2
]).

-include_lib("osc/include/osc_types.hrl").
-include_lib("osc_meta/include/osc_meta.hrl").

create(MetricID, {rectangular, Aggregation, Interval, Count}) ->
    SQL = <<
        "INSERT INTO windows"
        " (metric_id, type, aggregation, interval, count)"
        " VALUES ($1, $2, $3, $4, $5)"
        " RETURNING id;"
    >>,
    Params = [
        MetricID,
        term_to_binary(rectangular),
        term_to_binary(Aggregation),
        Interval,
        Count
    ],
    {ok, 1, _, [{ID}]} = osc_sql:adhoc(SQL, Params),
    {ok, ID}.


-spec lookup(MetricID) -> Windows when
    MetricID :: metric_id(),
    Windows :: [windowmeta()].

lookup(MetricID) ->
    WindowSQL = <<
        "SELECT id, type, aggregation, interval, count"
        " FROM windows WHERE metric_id = $1;"
    >>,
    {ok, _, Windows} = osc_sql:adhoc(WindowSQL, [MetricID]),
    lists:map(
        fun({ID, Type, Aggregation, Interval, Count}) ->
            WindowMeta = #windowmeta{
                id = ID,
                metric_id = MetricID,
                window_type = binary_to_term(Type),
                aggregation = binary_to_term(Aggregation),
                interval = Interval,
                count = Count
            },
            refresh(WindowMeta)
        end,
        Windows
    ).


-spec refresh(WindowMeta) -> WindowMeta when
    WindowMeta :: windowmeta().

refresh(WindowMeta) ->
    %% N.B.: This only refreshes persisted values, because that's the only field
    %% that's allowed to be mutable at the moment.
    PersistSQL = <<
        "SELECT timestamp, count"
        " FROM persists WHERE window_id = $1;"
    >>,
    {ok, _, Persisted} = osc_sql:adhoc(PersistSQL, [WindowMeta#windowmeta.id]),
    WindowMeta#windowmeta{persisted = Persisted}.

id(#windowmeta{id=ID}) ->
    ID.

aggregation(#windowmeta{aggregation=Aggregation}) ->
    Aggregation.

interval(#windowmeta{interval=Interval}) ->
    Interval.

count(#windowmeta{count=Count}) ->
    Count.

persisted(#windowmeta{persisted=Persisted}) ->
    Persisted.

earliest_persisted_time(#windowmeta{persisted=[]}) ->
    undefined;
earliest_persisted_time(#windowmeta{persisted=[{Timestamp, _}|_]}) ->
    Timestamp.

latest_persisted_time(#windowmeta{persisted=[]}) ->
    undefined;
latest_persisted_time(#windowmeta{interval=Interval, persisted=Persisted}) ->
    {Timestamp, Count} = lists:last(Persisted),
    Timestamp + Interval * (Count - 1).

insert_persist(Window, Timestamp, Count) ->
    {ok, _Count} = osc_sql:named(
        insert_persist, [Window#windowmeta.id, Timestamp, Count]
    ),
    ok.

delete_persist(Window, Timestamp) ->
    {ok, _Count} = osc_sql:named(
        delete_persist, [Window#windowmeta.id, Timestamp]
    ),
    ok.
