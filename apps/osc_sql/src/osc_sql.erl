-module(osc_sql).

-export([adhoc/2, named/2, batch/1, reload_statements/0]).

adhoc(SQL, Fields) ->
    folsom_metrics:notify({osc_sql, adhoc_queries}, {inc, 1}),
    transact({adhoc, SQL, Fields}).

named(Smt, Fields) ->
    folsom_metrics:notify({osc_sql, named_queries}, {inc, 1}),
    transact({named, Smt, Fields}).

batch(SmtFields) ->
    folsom_metrics:notify({osc_sql, batch_queries}, {inc, 1}),
    transact({batch, SmtFields}).

reload_statements() ->
    %% Poolboy doesn't expose get_all_workers, so hack it in here
    %% Return type matches supervisor:which_children/1
    Workers = gen_server:call(pgsql, get_all_workers),
    lists:foreach(fun({undefined, Pid, worker, [osc_sql_worker]}) ->
        ok = gen_server:call(Pid, reload_statements)
    end, Workers).

transact(Msg) ->
    transact(Msg, 1000, 30000).

transact(Message, Backoff, Timeout) when Timeout > 0 ->
    try poolboy:checkout(pgsql, false, Timeout) of
        full ->
            timer:sleep(min(Backoff, Timeout)),
            transact(Message, Backoff * 1.5, Timeout - Backoff);
        Worker ->
            try
                gen_server:call(Worker, Message, Timeout)
            catch exit:{timeout, _} ->
                timeout
            after
                ok = poolboy:checkin(pgsql, Worker)
            end
    catch exit:{timeout, _} ->
        timeout
    end;
transact(_, _, _) ->
    timeout.
