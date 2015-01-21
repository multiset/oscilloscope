-module(osc_persistence_worker).
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

-record(st, {}).


start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


init(_) ->
    lager:debug("Starting persistence worker ~p", [self()]),
    {ok, #st{}}.


handle_call({persist, WindowMeta, Window}, _From, State) ->
    {reply, osc_persistence:persist_int(WindowMeta, Window), State};
handle_call({read, WindowMeta, From, Until}, _From, State) ->
    {reply, osc_persistence:read_int(WindowMeta, From, Until), State};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, error, State}.


handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.


handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


