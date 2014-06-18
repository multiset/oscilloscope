-module(oscilloscope_persistence_fsm_sup).
-behavior(supervisor).

-export([spawn/1, start_link/0, init/1]).

spawn(Args) ->
    supervisor:start_child(?MODULE, Args).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Spec = {
        ignored, {oscilloscope_persistence_fsm, start_link, []},
        temporary, 5000, worker, [oscilloscope_persistence_fsm]
    },
    {ok, {{simple_one_for_one, 10, 10}, [Spec]}}.