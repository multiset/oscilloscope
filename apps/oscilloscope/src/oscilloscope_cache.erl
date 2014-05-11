-module(oscilloscope_cache).

-export([
    new/2,
    update/2,
    read/3
]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

-record(cache, {
    aggregation,
    resolutions
}).

new(Aggregation, Resolutions) ->
    #cache{aggregation=Aggregation, resolutions=Resolutions}.

update(_Props, Cache) ->
    Cache.

-spec read(timestamp(), timestamp(), any()) -> read().
read(From, Until, Cache) ->
    [R|_Rs] = Cache#cache.resolutions,
    {From, Until, R, []}.
