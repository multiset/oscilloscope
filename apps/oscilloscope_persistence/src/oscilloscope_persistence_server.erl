-module(oscilloscope_persistence_server).

-behaviour(gen_server).

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
    commutator,
    min_chunk_size,
    max_chunk_size,
    min_persist_age
}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init({Commutator, MinChunkSize, MaxChunkSize, MinPersistAge}) ->
    {ok, #st{
        commutator=Commutator,
        min_chunk_size=MinChunkSize,
        max_chunk_size=MaxChunkSize,
        min_persist_age=MinPersistAge
    }}.

handle_call({persist, CacheId, T0, Points, AggFun}, _From, State) ->
    {reply, {ok, []}, State};
handle_call({vacuum, CacheId, Timestamps}, _From, State) ->
    {reply, {ok, []}, State};
handle_call({read, CacheId, StartTime, EndTime}, _From, State) ->
    {reply, {ok, []}, State};
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
