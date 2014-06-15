-module(oscilloscope_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    VNodeMaster = {
        oscilloscope_vnode_master,
        {riak_core_vnode_master, start_link, [oscilloscope_vnode]},
        permanent, 5000, worker, [riak_core_vnode_master]
    },
    ReadFsmSup = {
        oscilloscope_read_fsm_sup,
        {oscilloscope_read_fsm_sup, start_link, []},
        permanent, infinity, supervisor, [oscilloscope_read_fsm_sup]
    },
    UpdateFsmSup = {
        oscilloscope_update_fsm_sup,
        {oscilloscope_update_fsm_sup, start_link, []},
        permanent, infinity, supervisor, [oscilloscope_update_fsm_sup]
    },
    {ok, {{one_for_one, 10, 10}, [VNodeMaster, ReadFsmSup, UpdateFsmSup]}}.

