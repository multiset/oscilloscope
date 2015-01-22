-module(osc_persistence_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    mstat:new_counter([osc_persistence, persist_attempts]),
    mstat:new_counter([osc_persistence, no_chunks_found]),
    mstat:new_counter([osc_persistence, persisted_chunks]),
    mstat:new_counter([osc_persistence, persisted_points]),
    mstat:new_histogram([osc_persistence, chunk_size]),
    {ok, Size} = application:get_env(osc_persistence, worker_pool_size),
    {ok, Overflow} = application:get_env(osc_persistence, worker_pool_overflow),
    Args = [
        {name, {local, osc_persistence_pool}},
        {worker_module, osc_persistence_worker},
        {size, Size},
        {max_overflow, Overflow}
    ],
    WorkerPool = poolboy:child_spec(osc_persistence_pool, Args, []),
    {ok, {{one_for_one, 100, 1}, [WorkerPool]}}.
