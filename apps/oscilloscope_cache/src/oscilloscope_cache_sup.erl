-module(oscilloscope_cache_sup).

-behaviour(supervisor).

-export([spawn_cache/1]).
-export([start_link/0, init/1]).

spawn_cache(Metric) ->
    supervisor:start_child(?MODULE, [Metric]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(Args) ->
    CacheSpec = {
        oscilloscope_cache,
        {oscilloscope_cache, start_link, Args},
        temporary, 5000, worker, [oscilloscope_cache]
    },
    {ok, {{simple_one_for_one, 10, 10}, [CacheSpec]}}.
