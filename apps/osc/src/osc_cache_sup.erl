-module(osc_cache_sup).

-behaviour(supervisor).

-export([find/1]).
-export([start_link/0, init/1]).

-include_lib("osc/include/osc_types.hrl").

-spec find(Metric) -> {ok, Pid} when
    Metric :: metric(),
    Pid :: pid().

find(Metric) ->
    case gproc:where({n, l, Metric}) of
        undefined ->
            Meta = case osc_meta_metric:lookup(Metric) of
                {ok, M} ->
                    M;
                not_found ->
                    {ok, M} = osc_meta_metric:create(Metric),
                    M
            end,
            Spec = {
                Metric,
                {osc_cache, start_link, [Meta]},
                temporary, 5000, worker, [osc_cache]
            },
            {ok, Pid} = supervisor:start_child(?MODULE, Spec),
            gproc:reg({n, l, Metric}, ignored),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{one_for_one, 10, 10}, []}}.
