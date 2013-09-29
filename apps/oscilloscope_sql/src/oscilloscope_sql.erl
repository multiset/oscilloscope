-module(oscilloscope_sql).

-export([start/0, stop/0]).
-export([adhoc/2, named/2]).

start() ->
    application:start(oscilloscope_sql).

stop() ->
    application:stop(oscilloscope_sql).

adhoc(SQL, Fields) ->
    transact({adhoc, SQL, Fields}).

named(Smt, Fields) ->
    % TODO: instrument me
    transact({named, Smt, Fields}).

transact(Msg) ->
    poolboy:transaction(
      pgsql,
      fun(Worker) -> gen_server:call(Worker, Msg) end).
