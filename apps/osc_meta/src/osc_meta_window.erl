-module(osc_meta_window).

-export([
    lookup/1,
    refresh/1,
    id/1,
    aggregation/1,
    interval/1,
    count/1,
    persisted/1,
    average_persist_size/1,
    type/1,
    earliest_persisted_time/1,
    latest_persisted_time/1,
    insert_persist/3,
    delete_persist/2
]).

-include_lib("osc/include/osc_types.hrl").
-include_lib("osc_meta/include/osc_meta.hrl").

-opaque window_id() :: pos_integer().
-opaque window_type() :: rectangular.

-record(windowmeta, {
    id :: window_id(),
    metric_id :: metric_id(),
    window_type :: window_type(),
    aggregation :: aggregation(),
    interval :: interval(),
    count :: count(),
    persisted :: persisted()
}).

-opaque windowmeta() :: #windowmeta{}.

-export_type([window_id/0, window_type/0, windowmeta/0]).

-spec lookup(MetricID) -> Windows when
    MetricID :: metric_id(),
    Windows :: [windowmeta()].

lookup(MetricID) ->
    SQL = "SELECT id, type, aggregation, interval, count "
          "FROM windows WHERE metric_id = $1;",
    {ok, _, Windows} = mpgsql:equery(SQL, [MetricID]),
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
    SQL = "SELECT timestamp, count FROM persists "
          "WHERE window_id = $1 AND vacuumed=FALSE;",
    {ok, _, Persisted} = mpgsql:equery(SQL, [WindowMeta#windowmeta.id]),
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


average_persist_size(#windowmeta{persisted=[]}) ->
    undefined;
average_persist_size(#windowmeta{persisted=Persisted}) ->
    Sum = lists:foldl(fun({_, N}, Acc) -> Acc + N end, 0, Persisted),
    Sum / length(Persisted).


type(#windowmeta{window_type=Type}) ->
    Type.


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
    SQL = "INSERT INTO persists "
          "(window_id, timestamp, count) "
          "VALUES ($1, $2, $3);",
    {ok, _Count} = mpgsql:equery(
        SQL, [Window#windowmeta.id, Timestamp, Count]
    ),
    ok.


delete_persist(Window, Timestamp) ->
    SQL = "UPDATE persists "
          "SET (vacuumed, vacuum_time)=(TRUE, (now() at time one 'utc')) "
          "WHERE window_id = $1 AND timestamp = $2;",
    {ok, _Count} = mpgsql:equery(
        SQL, [Window#windowmeta.id, Timestamp]
    ),
    ok.
