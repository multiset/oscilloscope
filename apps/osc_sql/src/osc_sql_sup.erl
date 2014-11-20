-module(osc_sql_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    folsom_metrics:new_counter({osc_sql, worker_inits}),
    folsom_metrics:new_counter({osc_sql, adhoc_queries}),
    folsom_metrics:new_counter({osc_sql, named_queries}),
    {ok, Pools} = application:get_env(osc_sql, pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [
            {name, {local, Name}},
            {worker_module, osc_sql_worker}
        ] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),
    {ok, {{one_for_one, 5, 10}, PoolSpecs}}.
