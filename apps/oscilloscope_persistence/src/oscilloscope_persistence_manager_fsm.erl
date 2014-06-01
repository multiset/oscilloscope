-module(oscilloscope_persistence_manager_fsm).
-behavior(gen_fsm).

-compile([{parse_transform, lager_transform}]).

-export([
    set_rate/1,
    set_target/1
]).

-export([
    make_request/2,
    waiting/2
]).

-export([
    start_link/0,
    init/1,
    handle_event/3,
    handle_info/3,
    handle_sync_event/4,
    code_change/4,
    terminate/3
]).

-record(st, {
    pending :: list(), %% Requests currently outstanding
    target :: non_neg_integer(), %% Target number of requests outstanding
    rate :: non_neg_integer() %% How long (ms) to wait between making requests
}).

set_rate(Rate) when is_integer(Rate) ->
    gen_fsm:sync_send_all_state_event(?MODULE, {set_rate, Rate}).

set_target(Target) when is_integer(Target) ->
    gen_fsm:sync_send_all_state_event(?MODULE, {set_target, Target}).

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, make_request, #st{pending=[], target=3, rate=100}, 0}.

make_request(timeout, #st{pending=P0, target=T}=State) when length(P0) < T ->
    {ok, ReqID, Pid} = oscilloscope_persistence_fsm:persist(),
    Ref = erlang:monitor(process, Pid),
    P1 = [{Ref, ReqID, Pid}|P0],
    {next_state, make_request, State#st{pending=P1}, State#st.rate};
make_request(timeout, State) ->
    {next_state, waiting, State}.

waiting(Msg, State) ->
    lager:error(
        "Received a send_event/1 message in ~p: ~p~n. This shouldn't happen",
        [Msg]
    ),
    {next_state, waiting, State}.

handle_info({'DOWN', Ref, process, _Pid, Reason}, waiting, State) ->
    #st{pending=P0, rate=Rate} = State,
    lager:warning("Persistence fsm crashed: ~p", Reason),
    P1 = lists:keydelete(Ref, 1, P0),
    {next_state, make_request, State#st{pending=P1}, Rate};
handle_info({ReqID, _Response}, waiting, State) ->
    #st{pending=P0, rate=Rate} = State,
    P1 = lists:keydelete(ReqID, 2, P0),
    {next_state, make_request, State#st{pending=P1}, Rate};
handle_info(Msg, _StateName, StateData) ->
    {stop, {unknown_info, Msg}, StateData}.

handle_event(Event, _StateName, StateData) ->
    {stop, {unknown_event, Event}, StateData}.

handle_sync_event({set_rate, Rate}, _From, _StateName, State) ->
    {reply, {ok, Rate}, make_request, State#st{rate=Rate}, 0};
handle_sync_event({set_target, Target}, _From, _StateName, State) ->
    {reply, {ok, Target}, make_request, State#st{target=Target}, 0};
handle_sync_event(Event, _From, _StateName, StateData) ->
    {stop, {unknown_sync_event, Event}, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

terminate(_Reason, _SN, _SD) ->
    ok.
