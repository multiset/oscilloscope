-module(oscilloscope_cache_sup).

-behaviour(supervisor).

-export([spawn_cache/1]).
-export([start_link/0, init/1]).

spawn_cache(Group) ->
    supervisor:start_child(?MODULE, [Group]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(Args) ->
    CacheSpec = {
        oscilloscope_cache_group_sup,
        {oscilloscope_cache_group_sup, start_link, Args},
        temporary, 5000, supervisor, [oscilloscope_cache_group_sup]
    },
    {ok, {{simple_one_for_one, 10, 10}, [CacheSpec]}}.
