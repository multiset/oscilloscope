-module(osc_cache_sup).

-behaviour(supervisor).

-export([start_cache/2]).

-export([start_link/0, init/1]).

-include_lib("osc/include/osc_types.hrl").
-include_lib("osc_meta/include/osc_meta.hrl").


start_cache(Metric, Meta) ->
    Spec = {
        Metric,
        {osc_cache, start_link, [Metric, Meta]},
        temporary, 5000, worker, [osc_cache]
    },
    supervisor:start_child(?MODULE, Spec).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    % Currently, there is no logic in oscilloscope to recover after a single
    % cache crash. Setting the max restarts to zero will cause a single cache
    % crash to kill the entire node, which will force oscilloscope to re-read
    % everything from kafka.
    {ok, {{one_for_one, 0, 10}, []}}.
