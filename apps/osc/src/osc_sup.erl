-module(osc_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    folsom_metrics:new_counter({osc, cache_updates}),
    CacheSup = {
        osc_cache_sup,
        {osc_cache_sup, start_link, []},
        permanent, infinity, supervisor, [osc_cache_sup]
    },
    {ok, {{one_for_one, 0, 10}, [CacheSup]}}.

