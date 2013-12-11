-module(oscilloscope_sql_worker).
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

init(Args) ->
    %% TODO: pass a tuple instead of a proplist?
    Hostname = proplists:get_value(hostname, Args),
    Port = proplists:get_value(port, Args),
    Database = proplists:get_value(database, Args),
    {ok, C} = pgsql:connect(Hostname, [{port, Port}, {database, Database}]),
    SmtPath = filename:join([
        code:lib_dir(oscilloscope_sql),
        "priv",
        proplists:get_value(statements, Args)
    ]),
    {ok, Statements} = file:consult(SmtPath),
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
