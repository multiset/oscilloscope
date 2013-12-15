-module(oscilloscope_net_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    folsom_metrics:new_counter({oscilloscope_net, inits, graphite}),
    folsom_metrics:new_counter({oscilloscope_net, recvs, tcp}),
    folsom_metrics:new_counter({oscilloscope_net, recvs, graphite}),
    {ok, {{one_for_one, 5, 10}, []}}.

