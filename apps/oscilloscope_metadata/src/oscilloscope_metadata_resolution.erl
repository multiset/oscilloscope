-module(oscilloscope_metadata_resolution).

-export([
    id/1,
    interval/1,
    persisted/1,
    insert_persist/3,
    delete_persist/2
]).

-export_type([resolution/0]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

-type resolution() :: {resolution_id(), interval(), count(), persisted()}.

id({ID, _, _, _}) ->
    ID.

interval({_, Interval, _, _}) ->
    Interval.

persisted({_, _, _, Persisted}) ->
    Persisted.

insert_persist(Resolution, Timestamp, Count) ->
    {ID, _, _, _} = Resolution,
    {ok, _Count} = oscilloscope_metadata_sql:named(
        insert_persist, [ID, Timestamp, Count]
    ),
    ok.

delete_persist(Resolution, Timestamp) ->
    {ID, _, _, _} = Resolution,
    {ok, _Count} = oscilloscope_metadata_sql:named(
        delete_persist, [ID, Timestamp]
    ),
    ok.
