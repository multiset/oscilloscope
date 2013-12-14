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
            %% TODO: get defaults from somewhere
            AggregationFun = avg,
            Resolutions = [{10, 1000, []}, {60, 1000, []}, {3600, 1000, []}],
            ok = oscilloscope_sql_metrics:create(
                User, Name, Host, AggregationFun, Resolutions
            ),
            get_or_create_resolutions(User, Name, Host)
    end.

-spec generate_spec(group(), aggregation(), resolution()) -> child_spec().
generate_spec(Group, AggregationFun, {Id, Interval, Count, Persisted}=Res) ->
    Args = {Group, Id, Interval, Count, Persisted, AggregationFun},
    {
        Res,
        {oscilloscope_cache, start_link, [Args]},
        permanent, 5000, worker, [oscilloscope_cache]
    }.
