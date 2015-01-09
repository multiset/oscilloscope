-module(osc_kafka_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, Partitions} = application:get_env(osc_kafka, partitions),
    PartitionSpecs = lists:map(fun(Partition) ->
        {
            osc_kafka_partition:name(Partition),
            {osc_kafka_partition, start_link, [Partition]},
            permanent,
            5000,
            worker,
            [osc_kafka_partition]
        }
    end, Partitions),
    MetricCreatorSup = ?CHILD(osc_kafka_metric_creator_sup, supervisor),
    {ok, {{one_for_one, 0, 1}, [
        ?CHILD(osc_kafka_router_sup, supervisor),
        MetricCreatorSup,
        PartitionSpecs
    ]}}.
