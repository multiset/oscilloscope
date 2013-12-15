-module(oscilloscope_cache_group_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init({User, Name, Host}=Group) ->
    ok = pg2:create(Group),
    {AF, Resolutions} = get_or_create_resolutions(User, Name, Host),
    Specs = lists:map(fun(R) -> generate_spec(Group, AF, R) end, Resolutions),
    {ok, {{one_for_all, 10, 10}, Specs}}.

-spec get_or_create_resolutions(binary(), binary(), binary()) ->
    {atom(), [resolution()]}.
get_or_create_resolutions(User, Name, Host) ->
    folsom_metrics:notify(
        {oscilloscope_cache, group_spawns},
        {inc, 1}
    ),
    case oscilloscope_sql_metrics:get(User, Name, Host) of
        {ok, Values} ->
            Values;
        {error, not_found} ->
            folsom_metrics:notify(
                {oscilloscope_cache, group_creations},
                {inc, 1}
            ),
            {AggregationFun, Resolutions} = get_metric_configuration(
                User, Name, Host
            ),
            ok = oscilloscope_sql_metrics:create(
                User, Name, Host, AggregationFun, Resolutions
            ),
            get_or_create_resolutions(User, Name, Host)
    end.

-spec generate_spec(group(), aggregation(), resolution()) -> child_spec().
generate_spec(Group, AggregationFun, {Id, Interval, Count, Persisted}=Res) ->
    {ok, Table} = application:get_env(oscilloscope_cache, dynamo_table),
    {ok, Schema} = application:get_env(oscilloscope_cache, dynamo_schema),
    {ok, Region} = application:get_env(oscilloscope_cache, dynamo_region),
    {ok, AccessKey} = application:get_env(oscilloscope_cache, dynamo_accesskey),
    {ok, SecretKey} = application:get_env(oscilloscope_cache, dynamo_secretkey),
    Commutator = [
        {table, Table},
        {schema, Schema},
        {region, Region},
        {accesskey, AccessKey},
        {secretkey, SecretKey}
    ],
    {ok, MinChunkSize} = application:get_env(
        oscilloscope_cache,
        min_chunk_size
    ),
    {ok, MaxChunkSize} = application:get_env(
        oscilloscope_cache,
        max_chunk_size
    ),
    {ok, MinPersistAge} = application:get_env(
        oscilloscope_cache,
        min_persist_age
    ),
    Args = {
        Group,
        Id,
        Interval,
        Count,
        Persisted,
        AggregationFun,
        Commutator,
        MinChunkSize,
        MaxChunkSize,
        MinPersistAge
    },
    {
        Res,
        {oscilloscope_cache, start_link, [Args]},
        permanent, 5000, worker, [oscilloscope_cache]
    }.

get_metric_configuration(_User, _Name, _Host) ->
    %% TODO: get from database
    {ok, AggregationFun} = application:get_env(
        oscilloscope_cache,
        default_aggregation_fun
    ),
    {ok, Resolutions} = application:get_env(
        oscilloscope_cache,
        default_resolutions
    ),
    {AggregationFun, Resolutions}.
