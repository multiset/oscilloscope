-module(oscilloscope_cache_sup).

-behaviour(supervisor).

-export([spawn_cache/1]).
-export([start_link/0, init/1]).

spawn_cache(Group) ->
    CacheSpec = {
        Group,
        {oscilloscope_cache_group_sup, start_link, [Group]},
        temporary, 5000, supervisor, [oscilloscope_cache_group_sup]
    },
    supervisor:start_child(?MODULE, CacheSpec).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    folsom_metrics:new_counter({oscilloscope_cache, cache_inits}),
    folsom_metrics:new_counter({oscilloscope_cache, mem_inits}),
    folsom_metrics:new_counter({oscilloscope_cache, reads}),
    folsom_metrics:new_counter({oscilloscope_cache, persistent_reads}),
    folsom_metrics:new_counter({oscilloscope_cache, points_read}),
    folsom_metrics:new_counter({oscilloscope_cache, points_processed}),
    folsom_metrics:new_counter({oscilloscope_cache, persists}),
    folsom_metrics:new_counter({oscilloscope_cache, null_persists}),
    folsom_metrics:new_counter({oscilloscope_cache, points_persisted}),
    folsom_metrics:new_counter({oscilloscope_cache, memory_cache_reads}),
    folsom_metrics:new_counter({oscilloscope_cache, memory_cache_writes}),
    folsom_metrics:new_counter({oscilloscope_cache, group_spawns}),
    folsom_metrics:new_counter({oscilloscope_cache, group_creations}),

    folsom_metrics:new_histogram(
        {oscilloscope_cache, points_per_chunk},
        slide_uniform,
        {60, 1028}
    ),
    folsom_metrics:new_histogram(
        {oscilloscope_cache, bytes_per_chunk},
        slide_uniform,
        {60, 1028}
    ),
    {ok, {{one_for_one, 10, 10}, []}}.
