-module(oscilloscope_sql).

-export([start/0, stop/0]).
-export([adhoc/2, named/2]).

start() ->
    application:start(oscilloscope_sql).

stop() ->
    application:stop(oscilloscope_sql).

adhoc(SQL, Fields) ->
    folsom_metrics:notify({oscilloscope_sql, adhoc_queries}, {inc, 1}),
    transact({adhoc, SQL, Fields}).

named(Smt, Fields) ->
    folsom_metrics:notify({oscilloscope_sql, named_queries}, {inc, 1}),
    transact({named, Smt, Fields}).

transact(Msg) ->
    poolboy:transaction(
      pgsql,
      fun(Worker) -> gen_server:call(Worker, Msg) end).
