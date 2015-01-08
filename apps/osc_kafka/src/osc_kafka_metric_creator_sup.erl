-module(osc_kafka_metric_creator_sup).

-behaviour(supervisor).

-export([start_child/2]).

-export([start_link/0]).

-export([init/1]).


start_child(OwnerID, Name) ->
    ChildSpec = {
        make_ref(),
        {osc_kafka_metric_creator, start_link, [OwnerID, Name]},
        transient,
        5000,
        worker,
        [osc_kafka_metric_creator]
    },
    supervisor:start_child(?MODULE, ChildSpec).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, {{one_for_one, 0, 1}, []}}.
