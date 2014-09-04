-module(osc_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    VNodeMaster = {
        osc_vnode_master,
        {riak_core_vnode_master, start_link, [osc_vnode]},
        permanent, 5000, worker, [riak_core_vnode_master]
    },
    ReadFsmSup = {
        osc_read_fsm_sup,
        {osc_read_fsm_sup, start_link, []},
        permanent, infinity, supervisor, [osc_read_fsm_sup]
    },
    UpdateFsmSup = {
        osc_update_fsm_sup,
        {osc_update_fsm_sup, start_link, []},
        permanent, infinity, supervisor, [osc_update_fsm_sup]
    },
    {ok, {{one_for_one, 10, 10}, [VNodeMaster, ReadFsmSup, UpdateFsmSup]}}.

