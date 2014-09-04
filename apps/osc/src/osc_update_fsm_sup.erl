-module(osc_update_fsm_sup).
-behavior(supervisor).

-export([spawn/1, start_link/0, init/1]).

spawn(Args) ->
    supervisor:start_child(?MODULE, Args).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Spec = {
        ignored, {osc_update_fsm, start_link, []},
        temporary, 5000, worker, [osc_update_fsm]
    },
    {ok, {{simple_one_for_one, 10, 10}, [Spec]}}.
