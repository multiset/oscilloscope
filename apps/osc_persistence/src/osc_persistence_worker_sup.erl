-module(osc_persistence_worker_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_child/1]).


start_child(MetricID, WindowID, MinSize) ->
    supervisor:start_child(?MODULE, [MetricID, WindowID, MinSize]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, {
        {simple_one_for_one, 0, 10}, [{
            osc_persistence_worker,
            {osc_persistence_worker, start_link, []},
            transient,
            1000,
            worker,
            [osc_persistence_worker]
        }]
    }}.
