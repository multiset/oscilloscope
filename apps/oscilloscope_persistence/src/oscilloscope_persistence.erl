-module(oscilloscope_persistence).

-export([
    maybe_persist/2,
    maybe_vacuum/4,
    read/3
]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

-spec maybe_persist(resolution_id(), timestamp(), array()) -> nil | any().
maybe_persist(Id, StartTime, Points) ->
    %% TODO
    nil.

-spec maybe_vacuum(resolution_id(), [timestamp()]) -> nil | any().
maybe_vacuum(Id, Timestamps) ->
    %% TODO
    nil.

-spec read(resolution_id(), pos_integer(), pos_integer()) -> {ok, list()}.
read(CacheID, StartTime, EndTime) ->
    %% TODO
    {ok, []}.
