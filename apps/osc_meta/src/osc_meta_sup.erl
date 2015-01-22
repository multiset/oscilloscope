-module(osc_meta_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    mstat:new_counter([osc_meta, creations, user]),
    mstat:new_counter([osc_meta, creations, org]),
    mstat:new_counter([osc_meta, creations, team]),
    mstat:new_counter([osc_meta, creations, metric]),
    mstat:new_counter([osc_meta, creations, window]),
    mstat:new_counter([osc_meta, creations, window_configuration]),
    {ok, {{one_for_one, 5, 10}, []}}.

