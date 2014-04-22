-module(oscilloscope_persistence_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Table} = application:get_env(oscilloscope_persistence, dynamo_table),
    {ok, Schema} = application:get_env(oscilloscope_persistence, dynamo_schema),
    {ok, Region} = application:get_env(oscilloscope_persistence, dynamo_region),
    {ok, AccessKey} = application:get_env(
        oscilloscope_persistence,
        dynamo_access_key
    ),
    {ok, SecretKey} = application:get_env(
        oscilloscope_persistence,
        dynamo_secret_key
    ),
    {ok, Commutator} = commutator:init(
        Table,
        Schema,
        Region,
        AccessKey,
        SecretKey
    ),
    {ok, MinChunkSize} = application:get_env(
        oscilloscope_persistence,
        min_chunk_size
    ),
    {ok, MaxChunkSize} = application:get_env(
        oscilloscope_persistence,
        max_chunk_size
    ),
    Args = {
        Commutator,
        MinChunkSize,
        MaxChunkSize,
    },
    Children = [{
        oscilloscope_persistence_server,
        {oscilloscope_persistence_server, start_link, [Args]},
        permanent, 5000, worker, [oscilloscope_persistence_server]
    }],
    {ok, {{one_for_one, 5, 10}, Children}}.

