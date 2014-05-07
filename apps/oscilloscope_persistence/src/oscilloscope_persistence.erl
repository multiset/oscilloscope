-module(oscilloscope_persistence).

-export([
    start/0,
    stop/0,
    persist/2,
    vacuum/2,
    read/3
]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

start() ->
    application:start(oscilloscope_persistence).

stop() ->
    application:stop(oscilloscope_persistence).

-spec persist(resolution_id(), [{timestamp(), number()}]) -> nil | any().
persist(CacheId, Points) ->
    gen_server:call(
        oscilloscope_persistence_server,
        {persist, CacheId, Points},
        60000
    ).

-spec vacuum(resolution_id(), [timestamp()]) -> nil | any().
vacuum(CacheId, Timestamps) ->
    gen_server:call(
        oscilloscope_persistence_server,
        {vacuum, CacheId, Timestamps},
        60000
    ).

-spec read(resolution_id(), pos_integer(), pos_integer()) -> {ok, list()}.
read(CacheId, StartTime, EndTime) ->
    gen_server:call(
        oscilloscope_persistence_server,
        {read, CacheId, StartTime, EndTime},
        10000
    ).
