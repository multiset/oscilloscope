-module(osc_sql).

-export([adhoc/2, named/2, batch/1]).

adhoc(SQL, Fields) ->
    folsom_metrics:notify({osc_sql, adhoc_queries}, {inc, 1}),
    transact({adhoc, SQL, Fields}).

named(Smt, Fields) ->
    folsom_metrics:notify({osc_sql, named_queries}, {inc, 1}),
    transact({named, Smt, Fields}).

batch(SmtFields) ->
    folsom_metrics:notify({osc_sql, batch_queries}, {inc, 1}),
    transact({batch, SmtFields}).

transact(Msg) ->
    poolboy:transaction(
      pgsql,
      fun(Worker) -> gen_server:call(Worker, Msg) end).
