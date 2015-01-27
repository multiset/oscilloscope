-module(osc_kafka_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    mstat:new_counter([osc_kafka, points, received]),
    mstat:new_counter([osc_kafka, points, received_with_id]),
    mstat:new_counter([osc_kafka, points, received_without_id]),
    mstat:new_counter([osc_kafka, fetches, empty]),
    mstat:new_counter([osc_kafka, fetches, successful]),
    mstat:new_counter([osc_kafka, fetches, error]),
    mstat:new_histogram([osc_kafka, batch_size]),
    mstat:new_counter([osc_kafka, inserts, retries]),
    mstat:new_counter([osc_kafka, inserts, spawned]),
    mstat:new_histogram([osc_kafka, inserts, latency]),
    mstat:new_counter([osc_kafka, creators, spawned]),
    mstat:new_counter([osc_kafka, creators, points_proxied]),
    mstat:new_counter([osc_kafka, creators, requests_denied]),
    mstat:new_counter([osc_kafka, creators, successes]),
    mstat:new_counter([osc_kafka, creators, failures]),
    mstat:new_counter([osc_kafka, creators, create_attempts]),
    mstat:new_counter([osc_kafka, creators, create_races]),
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
    {ok, {{one_for_one, 0, 1}, [
        ?CHILD(osc_kafka_insert_sup, supervisor),
        ?CHILD(osc_kafka_metric_creator_sup, supervisor)|
        PartitionSpecs
    ]}}.
