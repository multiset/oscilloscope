-module(oscilloscope_persistence).

-export([
    persist/2,
    vacuum/2,
    read/3
]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

-spec persist(resolution_id(), [{timestamp(), number()}]) -> nil | any().
persist(CacheId, Points) ->
    gen_server:call(
        oscilloscope_persistence_server,
        {persist, CacheId, StartTime, Points, AggFun}
    ).

-spec vacuum(resolution_id(), [timestamp()]) -> nil | any().
vacuum(CacheId, Timestamps) ->
    gen_server:call(
        oscilloscope_persistence_server,
        {vacuum, CacheId, Timestamps}
    ).

-spec read(resolution_id(), pos_integer(), pos_integer()) -> {ok, list()}.
read(CacheId, StartTime, EndTime) ->
    gen_server:call(
        oscilloscope_persistence_server,
        {read, CacheId, StartTime, EndTime}
    ).
