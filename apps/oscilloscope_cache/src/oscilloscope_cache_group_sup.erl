-module(oscilloscope_cache_group_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

init(Key) ->
    folsom_metrics:notify(
        {oscilloscope_cache, group_spawns},
        {inc, 1}
    ),
    {AF, Resolutions} = get_or_create_group_configuration(Key),
    Specs = lists:map(fun(R) -> generate_spec(AF, R) end, Resolutions),
    {ok, {{one_for_all, 10, 10}, Specs}}.

-spec get_or_create_group_configuration(any()) ->
    {atom(), [resolution()]}.
get_or_create_group_configuration(Key) ->
    case oscilloscope_metadata_metrics:get(Key) of
        {ok, Values} ->
            Values;
        not_found ->
            folsom_metrics:notify(
                {oscilloscope_cache, group_creations},
                {inc, 1}
            ),
            {AggregationFun, Resolutions} = build_group_configuration(
                Key
            ),
            ok = oscilloscope_metadata_metrics:create(
                Key, AggregationFun, Resolutions
            ),
            get_or_create_group_configuration(Key)
    end.

-spec generate_spec(aggregation(), {resolution_id(), interval(), count(), persisted()}) -> child_spec().
generate_spec(AggregationFun, {ResID, Interval, Count, Persisted}) ->
    {ok, MinPersistAge} = application:get_env(
        oscilloscope_cache,
        min_persist_age
    ),
    Args = {
        ResID,
        Interval,
        Count,
        MinPersistAge,
        Persisted,
        AggregationFun
    },
    {
        ResID,
        {oscilloscope_cache, start_link, [Args]},
        permanent, 5000, worker, [oscilloscope_cache]
    }.

build_group_configuration(Key) ->
    {ok, AggConfigs} = oscilloscope_metadata_metrics:get_aggregation_configuration(),
    {ok, ResConfigs} = oscilloscope_metadata_metrics:get_resolution_configuration(),
    AggregationFun = case find_config_match(Key, AggConfigs) of
        nomatch -> get_default_aggregation_fun()
    end,
    Resolutions = case find_config_match(Key, ResConfigs) of
        nomatch -> get_default_resolutions()
    end,
    {AggregationFun, Resolutions}.

find_config_match(_Key, _Configs) ->
    %% TODO
    nomatch.

get_default_aggregation_fun() ->
    {ok, AggregationFun} = application:get_env(
        oscilloscope_cache,
        default_aggregation_fun
    ),
    AggregationFun.

get_default_resolutions() ->
    {ok, Resolutions} = application:get_env(
        oscilloscope_cache,
        default_resolutions
    ),
    Resolutions.
