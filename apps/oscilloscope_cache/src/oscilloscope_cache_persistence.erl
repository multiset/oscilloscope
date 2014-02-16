-module(oscilloscope_cache_persistence).

-export([
    persist/3,
    vacuum/3
]).

persist(ResolutionId, Points, Commutator) ->
    lists:map(
        fun({Timestamp, Value, Size}) ->
            %% TODO: this will badmatch if, e.g., we're rate-limited
            Start = erlang:now(),
            {ok, true} = commutator:put_item(
                Commutator,
                [ResolutionId, Timestamp, Value]
            ),
            Latency = timer:now_diff(erlang:now(), Start),
            folsom_metrics:notify(
                {oscilloscope_cache, persistent_store, write_latency, sliding},
                Latency
            ),
            folsom_metrics:notify(
                {oscilloscope_cache, persistent_store, write_latency, uniform},
                Latency
            ),
            ok = oscilloscope_sql_metrics:insert_persisted(
                ResolutionId,
                Timestamp,
                Size
            )
        end,
        Points
    ).

vacuum(ResolutionId, Points, Commutator) ->
    lists:map(
        fun(Timestamp) ->
           Start = erlang:now(),
           {ok, true} = commutator:delete_item(
               Commutator,
               [ResolutionId, Timestamp]
           ),
            Latency = timer:now_diff(erlang:now(), Start),
            folsom_metrics:notify(
                {oscilloscope_cache, persistent_store, delete_latency, sliding},
                Latency
            ),
            folsom_metrics:notify(
                {oscilloscope_cache, persistent_store, delete_latency, uniform},
                Latency
            ),
            ok = oscilloscope_sql_metrics:delete_persisted(
                ResolutionId,
                Timestamp
            )
        end,
        Points
    ).
