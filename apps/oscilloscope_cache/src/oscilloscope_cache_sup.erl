-module(oscilloscope_cache_sup).

-behaviour(supervisor).

-export([spawn_group/1, terminate_group/1, find_group/1, list_groups/0]).
-export([start_link/0, init/1]).

spawn_group(Metric) ->
    MetricSpec = {
        Metric,
        {oscilloscope_cache_group_sup, start_link, [Metric]},
        temporary, 5000, supervisor, [oscilloscope_cache_group_sup]
    },
    {ok, Pid} = supervisor:start_child(?MODULE, MetricSpec),
    MetricChildren = supervisor:which_children(Pid),
    lists:map(fun({_, P, _, _}) -> P end, MetricChildren).

terminate_group(Metric) ->
    folsom_metrics:notify({oscilloscope_cache, group_terminations}, {inc, 1}),
    supervisor:terminate_child(oscilloscope_cache_sup, Metric).

find_group(Metric) ->
    Children = supervisor:which_children(?MODULE),
    Match = lists:filter(fun({Id, _, _, _}) -> Id =:= Metric end, Children),
    case Match of
        [] ->
            not_found;
        [{Metric, Pid, _, _}] ->
            MetricChildren = supervisor:which_children(Pid),
            lists:map(fun({_, P, _, _}) -> P end, MetricChildren)
    end.

list_groups() ->
    Children = supervisor:which_children(?MODULE),
    lists:map(fun({Id, _, _, _}) -> Id end, Children).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    folsom_metrics:new_counter({oscilloscope_cache, cache_inits}),
    folsom_metrics:new_counter({oscilloscope_cache, mem_inits}),
    folsom_metrics:new_counter({oscilloscope_cache, reads}),
    folsom_metrics:new_counter({oscilloscope_cache, points_read}),
    folsom_metrics:new_counter({oscilloscope_cache, points_processed}),
    folsom_metrics:new_counter({oscilloscope_cache, group_spawns}),
    folsom_metrics:new_counter({oscilloscope_cache, group_creations}),
    folsom_metrics:new_counter({oscilloscope_cache, group_terminations}),

    folsom_metrics:new_histogram(
        {oscilloscope_cache, points_per_chunk, sliding},
        slide_uniform,
        {60, 1028}
    ),
    folsom_metrics:new_histogram(
        {oscilloscope_cache, points_per_chunk, uniform}
    ),
    folsom_metrics:new_histogram(
        {oscilloscope_cache, bytes_per_chunk, sliding},
        slide_uniform,
        {60, 1028}
    ),
    folsom_metrics:new_histogram(
        {oscilloscope_cache, bytes_per_chunk, uniform}
    ),
    {ok, {{one_for_one, 10, 10}, []}}.
