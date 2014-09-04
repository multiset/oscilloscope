-module(osc_metadata_sql).

-export([adhoc/2, named/2]).

adhoc(SQL, Fields) ->
    folsom_metrics:notify({osc_metadata, adhoc_queries}, {inc, 1}),
    transact({adhoc, SQL, Fields}).

named(Smt, Fields) ->
    folsom_metrics:notify({osc_metadata, named_queries}, {inc, 1}),
    transact({named, Smt, Fields}).

transact(Msg) ->
    poolboy:transaction(
      pgsql,
      fun(Worker) -> gen_server:call(Worker, Msg) end).
