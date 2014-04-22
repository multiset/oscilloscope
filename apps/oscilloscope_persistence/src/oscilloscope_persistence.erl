-module(oscilloscope_persistence).

-export([
    persist/2,
    vacuum/4,
    read/3
]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

-spec persist(resolution_id(), timestamp(), array()) -> nil | any().
persist(Id, StartTime, Points) ->
    %% TODO
    nil.

-spec vacuum(resolution_id(), [timestamp()]) -> nil | any().
vacuum(Id, Timestamps) ->
    %% TODO
    nil.

-spec read(resolution_id(), pos_integer(), pos_integer()) -> {ok, list()}.
read(CacheID, StartTime, EndTime) ->
    %% TODO
    {ok, []}.
