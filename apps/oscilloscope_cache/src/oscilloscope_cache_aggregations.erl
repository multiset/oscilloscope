-module(oscilloscope_cache_aggregations).

-export([avg/1]).

avg(Values) ->
    lists:sum(Values) / length(Values).
