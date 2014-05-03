-module(oscilloscope_metadata).

-export([start/0, stop/0]).
-export([adhoc/2, named/2]).

start() ->
    application:start(oscilloscope_metadata).

stop() ->
    application:stop(oscilloscope_metadata).

adhoc(SQL, Fields) ->
    folsom_metrics:notify({oscilloscope_metadata, adhoc_queries}, {inc, 1}),
    transact({adhoc, SQL, Fields}).

named(Smt, Fields) ->
    folsom_metrics:notify({oscilloscope_metadata, named_queries}, {inc, 1}),
    transact({named, Smt, Fields}).

transact(Msg) ->
    poolboy:transaction(
      pgsql,
      fun(Worker) -> gen_server:call(Worker, Msg) end).
