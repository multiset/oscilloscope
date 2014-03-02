-module(oscilloscope_cache_memory).
-export([read/1, write/2]).

-include("oscilloscope_cache.hrl").

-spec read(term()) -> not_found | any().
read(Key) ->
    Start = erlang:now(),
    try erp:q(["GET", term_to_binary(Key)]) of
        {ok, Item} ->
            Latency = timer:now_diff(erlang:now(), Start),
            folsom_metrics:notify(
                {oscilloscope_cache, memory_cache, read, latency, sliding},
                Latency
            ),
            folsom_metrics:notify(
                {oscilloscope_cache, memory_cache, read, latency, uniform},
                Latency
            ),
            case Item of
                undefined -> not_found;
                Value -> ?VALDECODE(Value)
            end
    catch exit:{timeout, _} ->
        folsom_metrics:notify(
            {oscilloscope_cache, memory_cache, timeouts},
            {inc, 1}
        ),
        timeout
    end.

-spec write(term(), term()) -> ok.
write(Key, Value) ->
    Start = erlang:now(),
    try erp:q(["SET", term_to_binary(Key), ?VALENCODE(Value)]) of
        {ok, <<"OK">>} ->
            Latency = timer:now_diff(erlang:now(), Start),
            folsom_metrics:notify(
                {oscilloscope_cache, memory_cache, write, latency, sliding},
                Latency
            ),
            folsom_metrics:notify(
                {oscilloscope_cache, memory_cache, write, latency, uniform},
            Latency
            ),
            ok
    catch exit:{timeout, _} ->
        folsom_metrics:notify(
            {oscilloscope_cache, memory_cache, timeouts},
            {inc, 1}
        ),
        timeout
    end.
