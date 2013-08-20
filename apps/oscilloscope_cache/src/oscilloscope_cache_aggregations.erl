-module(oscilloscope_cache_aggregations).

-export([avg/1]).

avg(Values) ->
    go(fun(Vs) -> lists:sum(Vs) / length(Vs) end, Values).

go(_Fun, null) ->
    null;
go(Fun, Values) ->
    Values1 = lists:filter(fun(V) -> V =/= null end, Values),
    Fun(Values1).
