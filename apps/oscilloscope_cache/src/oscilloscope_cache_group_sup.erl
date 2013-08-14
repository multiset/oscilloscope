-module(oscilloscope_cache_group_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Metric) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Metric).

init(Metric) ->
    ok = pg2:create(Metric),
    Resolutions = [{10, 1000}, {60, 1000}, {3600, 1000}],
    Specs = lists:map(
        fun(R) ->
            {
                R,
                {oscilloscope_cache, start_link, [Metric, R]},
                permanent, 5000, worker, [oscilloscope_cache]
            }
        end, Resolutions
    ),
    {ok, {{one_for_all, 0, 1}, Specs}}.
