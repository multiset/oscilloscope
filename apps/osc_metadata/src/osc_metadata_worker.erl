-module(osc_metadata_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(st, {
    conn,
    statements
}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(_) ->
    folsom_metrics:notify({osc_metadata, worker_inits}, {inc, 1}),
    {ok, Hostname} = application:get_env(osc_metadata, hostname),
    {ok, Port} = application:get_env(osc_metadata, port),
    {ok, Database} = application:get_env(osc_metadata, database),
    {ok, Username} = application:get_env(osc_metadata, username),
    {ok, Password} = application:get_env(osc_metadata, password),
    {ok, C} = pgsql:connect(
        Hostname,
        Username,
        Password,
        [{port, Port}, {database, Database}]
    ),
    {ok, StatementFile} = application:get_env(osc_metadata, statements),
    StatementPath = filename:join([
        code:lib_dir(osc_metadata),
        "priv",
        StatementFile
    ]),
    {ok, Statements} = file:consult(StatementPath),
    {ok, #st{conn=C, statements=Statements}}.

handle_call({adhoc, SQL, Fields}, _From, #st{conn=C}=State) ->
    {reply, pgsql:equery(C, SQL, Fields), State};
handle_call({named, Smt, Fields}, _From, #st{conn=C, statements=Smts}=State) ->
    SQL = proplists:get_value(Smt, Smts),
    {reply, pgsql:equery(C, SQL, Fields), State};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, error, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

terminate(_Reason, #st{conn = Conn}) ->
    ok = pgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
