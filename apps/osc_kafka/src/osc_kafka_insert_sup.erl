-module(osc_kafka_insert_sup).

-behaviour(supervisor).

-export([start_child/1]).

-export([start_link/0]).

-export([init/1]).


start_child(Batch) ->
    {ok, _Pid} = supervisor:start_child(?MODULE, [Batch]),
    ok.


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, {{simple_one_for_one, 0, 10}, [{
        ?MODULE,
        {osc_kafka_insert, start_link, []},
        transient,
        1000,
        worker,
        [?MODULE]
    }]}}.
