-module(oscilloscope_cache_group_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

init({Name, Host}) ->
    folsom_metrics:notify(
        {oscilloscope_cache, group_spawns},
        {inc, 1}
    ),
    {AF, Resolutions} = get_or_create_group_configuration(
        Name,
        Host
    ),
    Specs = lists:map(fun(R) -> generate_spec(AF, R) end, Resolutions),
    {ok, {{one_for_all, 10, 10}, Specs}}.

-spec get_or_create_group_configuration(service(), host()) ->
    {atom(), [resolution()]}.
get_or_create_group_configuration(Name, Host) ->
    case oscilloscope_metadata_metrics:get(Name, Host) of
        {ok, Values} ->
            Values;
        not_found ->
            folsom_metrics:notify(
                {oscilloscope_cache, group_creations},
                {inc, 1}
            ),
            {AggregationFun, Resolutions} = build_group_configuration(
                Name, Host
            ),
            ok = oscilloscope_metadata_metrics:create(
                Name, Host, AggregationFun, Resolutions
            ),
            get_or_create_group_configuration(Name, Host)
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

build_group_configuration(Service, Host) ->
    {ok, AggConfigs} = oscilloscope_metadata_metrics:get_aggregation_configuration(),
    {ok, ResConfigs} = oscilloscope_metadata_metrics:get_resolution_configuration(),
    AggregationFun = case find_config_match(Service, Host, AggConfigs) of
        nomatch -> get_default_aggregation_fun();
        AggMatch -> parse_aggregation_fun_match(AggMatch)
    end,
    Resolutions = case find_config_match(Service, Host, ResConfigs) of
        nomatch -> get_default_resolutions();
        ResMatch -> parse_resolution_match(ResMatch)
    end,
    {AggregationFun, Resolutions}.

find_config_match(_Service, _Host, []) ->
    nomatch;
find_config_match(Service, Host, [{ServiceRegex, HostRegex, Config}|Cs]) ->
    ServiceMatch = re:run(Service, ServiceRegex) =/= nomatch,
    HostMatch = re:run(Host, HostRegex) =/= nomatch,
    case ServiceMatch and HostMatch of
        true -> Config;
        false -> find_config_match(Service, Host, Cs)
    end.

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

parse_aggregation_fun_match(<<"avg">>) ->
    avg;
parse_aggregation_fun_match(_) ->
    get_default_aggregation_fun().

parse_resolution_match(Match) ->
    case oscilloscope_cache_util:parse_resolution(Match) of
        {error, _, _} ->
            get_default_resolutions();
        Resolutions ->
            Resolutions
    end.
