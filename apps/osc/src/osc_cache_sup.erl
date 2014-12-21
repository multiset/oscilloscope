-module(osc_cache_sup).

-behaviour(supervisor).

-export([find/1]).
-export([start_link/0, init/1]).

-include_lib("osc/include/osc_types.hrl").
-include_lib("osc_meta/include/osc_meta.hrl").

-spec find(Metric) -> {ok, Pid} | not_found when
    Metric :: metric_id(),
    Pid :: pid().

find(Metric) ->
    case gproc:where({n, l, Metric}) of
        undefined ->
            case osc_meta_metric:lookup(Metric) of
                not_found ->
                    not_found;
                {ok, Meta} ->
                    Spec = {
                        Metric,
                        {osc_cache, start_link, [Metric, Meta]},
                        temporary, 5000, worker, [osc_cache]
                    },
                    supervisor:start_child(?MODULE, Spec)
            end;
        Pid ->
            {ok, Pid}
    end.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{one_for_one, 10, 10}, []}}.
