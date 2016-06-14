-module(osc_persistence_event_listener).
-behaviour(gen_event).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(st, {
    events
}).


init([]) ->
    {ok, #st{events=[]}}.


handle_event({window_update, _, _, _, _}=E, State) ->
    {ok, #st{events=maybe_submit([E|State#st.events])}};
handle_event({cache_read, _, _, _}=E, State) ->
    {ok, #st{events=maybe_submit([E|State#st.events])}};
handle_event(_, State) ->
    {ok, State}.


handle_call(_, State) ->
    {ok, ok, State}.


handle_info(_, State) ->
    {ok, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(swap, State) ->
    State;
terminate(_Arg, _State) ->
    ok.


maybe_submit(Events) ->
    {ok, BatchSize} = application:get_env(osc_persistence, event_batch_size),
    case length(Events) < BatchSize of
        true ->
            Events;
        false ->
            ok = osc_persistence_manager:submit_events(Events),
            []
    end.

