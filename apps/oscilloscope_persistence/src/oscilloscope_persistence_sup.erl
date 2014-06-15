-module(oscilloscope_persistence_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    PersistenceManager = {
        oscilloscope_persistence_manager_fsm,
        {oscilloscope_persistence_manager_fsm, start_link, []},
        permanent, 5000, worker, [oscilloscope_persistence_manager_fsm]
    },
    PersistFsmSup = {
        oscilloscope_persistence_fsm_sup,
        {oscilloscope_persistence_fsm_sup, start_link, []},
        permanent, infinity, supervisor, [oscilloscope_persistence_fsm_sup]
    },
    {ok, {{one_for_one, 5, 10}, [PersistenceManager, PersistFsmSup]}}.
