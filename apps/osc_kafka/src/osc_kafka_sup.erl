-module(osc_kafka_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type, A), {I, {I, start_link, A}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, Partitions} = application:get_env(osc_kafka, partitions),
    PartitionSpecs = lists:map(fun(Partition) ->
        {
            list_to_atom("osc_kafka_partition_" ++ integer_to_list(Partition)),
            {osc_kafka_partition, start_link, [Partition]},
            permanent,
            5000,
            worker,
            [osc_kafka_partition]
        }
    end, Partitions),
    {ok, {{one_for_one, 5, 10}, PartitionSpecs}}.
