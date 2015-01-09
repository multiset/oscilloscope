-module(osc_kafka_router_sup).

-behaviour(supervisor).

-export([start_child/1]).

-export([start_link/0]).

-export([init/1]).


start_child(Batch) ->
    Spec = {
        make_ref(),
        {osc_kafka_router, start_link, [Batch]},
        permanent,
        5000,
        worker,
        [osc_kafka_router]
    },
    {ok, _Pid} = supervisor:start_child(?MODULE, Spec),
    ok.


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, {{one_for_one, 0, 10}, []}}.
