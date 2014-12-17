-module(osc_persistence_util).

-export([commutator/0]).


commutator() ->
    {ok, Table} = application:get_env(osc_persistence, dynamo_table),
    {ok, Schema} = application:get_env(osc_persistence, dynamo_schema),
    {ok, Region} = application:get_env(osc_persistence, dynamo_region),
    {ok, AccessKey} = application:get_env(
        osc_persistence,
        dynamo_access_key
    ),
    {ok, SecretKey} = application:get_env(
        osc_persistence,
        dynamo_secret_key
    ),
    {ok, Commutator} = commutator:init(
        Table,
        Schema,
        Region,
        AccessKey,
        SecretKey
    ),
    Commutator.
