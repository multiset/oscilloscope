-module(oscilloscope_persistence).

-export([
    maybe_persist/2,
    maybe_vacuum/4,
    read/3
]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

-spec maybe_persist(pos_integer(), array()) -> nil | any().
maybe_persist(StartTime, Points) ->
    %% TODO
    nil.

maybe_vacuum(T, Persisted, Interval, Count) ->
    %% TODO
    nil.

-spec read(resolution_id(), pos_integer(), pos_integer()) -> {ok, list()}.
read(CacheID, StartTime, EndTime) ->
    %% TODO
    {ok, []}.
