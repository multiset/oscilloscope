-module(osc_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    mstat:new_counter([osc, reads, points]),
    mstat:new_counter([osc, reads, undefined]),
    mstat:new_counter([osc, reads, persistent_only]),
    mstat:new_counter([osc, reads, cached_only]),
    mstat:new_counter([osc, reads, persistent_and_cached]),
    mstat:new_counter([osc, reads, count]),
    mstat:new_counter([osc, reads, successful]),
    mstat:new_counter([osc, cache, points_read]),
    mstat:new_counter([osc, cache, points_received]),
    mstat:new_counter([osc, cache, window_updates]),
    CacheSup = {
        osc_cache_sup,
        {osc_cache_sup, start_link, []},
        permanent, infinity, supervisor, [osc_cache_sup]
    },
    EventSup = {
        osc_event,
        {gen_event, start_link, [{local, osc_event}]},
        permanent, 5000, worker, [dynamic]
    },
    {ok, {{one_for_one, 0, 10}, [CacheSup, EventSup]}}.

