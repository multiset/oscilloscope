-module(osc_persistence_manager_fsm).
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
    pending = [] :: list(), %% Requests currently outstanding
    target = 1 :: non_neg_integer(), %% Target number of requests outstanding
    rate = 1000 :: non_neg_integer() %% How long (ms) to wait between requests
}).

set_rate(Rate) when is_integer(Rate) ->
    gen_fsm:sync_send_all_state_event(?MODULE, {set_rate, Rate}).

set_target(Target) when is_integer(Target) ->
    gen_fsm:sync_send_all_state_event(?MODULE, {set_target, Target}).

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Delay = 60000, % Delay the first attempt to allow the ring to prepare
    {ok, make_request, #st{pending=[], target=1, rate=1000}, Delay}.

make_request(timeout, #st{pending=P0, target=T}=State) when length(P0) < T ->
    P1 = case osc_persistence_fsm:persist() of
        {ok, ReqID, Pid} ->
            Ref = erlang:monitor(process, Pid),
            [{Ref, ReqID, Pid}|P0];
        {error, Reason} ->
            lager:warning(
                "Failed to start persistence_fsm because ~p",
                [Reason]
            ),
            P0
    end,
    {next_state, make_request, State#st{pending=P1}, State#st.rate};
make_request(timeout, State) ->
    {next_state, waiting, State}.

waiting(Msg, State) ->
    lager:error(
        "Received a send_event/1 message ~p~n. This shouldn't happen",
        [Msg]
    ),
    {next_state, waiting, State}.

handle_info({'DOWN', Ref, process, _Pid, Reason}, _StateName, State) ->
    #st{pending=P0, rate=Rate} = State,
    case Reason of
        normal -> ok;
        noproc -> ok; % Pid died before we monitored
        _ -> lager:warning("Persistence fsm crashed: ~p", [Reason])
    end,
    P1 = lists:keydelete(Ref, 1, P0),
    {next_state, make_request, State#st{pending=P1}, Rate};
handle_info({ReqID, _Response}, _StateName, State) ->
    #st{pending=P0, rate=Rate} = State,
    P1 = lists:keydelete(ReqID, 2, P0),
    {next_state, make_request, State#st{pending=P1}, Rate};
handle_info(Msg, StateName, StateData) ->
    {stop, {unknown_info, {Msg, StateName}}, StateData}.

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
