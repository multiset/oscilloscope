-module(oscilloscope_cache_memory).
-export([read/1, write/2]).

-include("oscilloscope_cache.hrl").

-spec read(term()) -> not_found | any().
read(Key) ->
    folsom_metrics:notify(
        {oscilloscope_cache, memory_cache_reads},
        {inc, 1}
    ),
    Start = erlang:now(),
    Response = case erp:q(["GET", term_to_binary(Key)]) of
        {ok, undefined} -> not_found;
        {ok, Value} -> ?VALDECODE(Value)
    end,
    folsom_metrics:notify(
        {oscilloscope_cache, memory_cache, read_latency},
        timer:now_diff(erlang:now(), Start)
    ),
    Response.

-spec write(term(), term()) -> ok.
write(Key, Value) ->
    folsom_metrics:notify(
        {oscilloscope_cache, memory_cache_writes},
        {inc, 1}
    ),
    Start = erlang:now(),
    {ok, <<"OK">>} = erp:q(["SET", term_to_binary(Key), ?VALENCODE(Value)]),
    folsom_metrics:notify(
        {oscilloscope_cache, memory_cache, write_latency},
        timer:now_diff(erlang:now(), Start)
    ),
    ok.
