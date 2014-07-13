-module(oscilloscope_entities_port_server).

-behaviour(gen_server).

-export([start_link/0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("oscilloscope_entities.hrl").

-record(state, {
    unused_ports,
    host
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, _, OwnerPorts} = oscilloscope_metadata_sql:named(get_used_ports, []),
    Ports = lists:foldl(fun({OwnerID, Port}, Acc) ->
        oscilloscope_entities_port:start(OwnerID, Port),
        [Port|Acc]
    end, [], OwnerPorts),
    UnusedPorts = lists:seq(1025, 65536) -- Ports,

    State = #state{
        unused_ports=UnusedPorts,
        host=(<<"localhost">>) % TODO: Make this a config
    },
    {ok, State}.

handle_call({create_port, OwnerID}, From, State) ->
    % hope we don't run out of ports. D:
    [Port|_] = State#state.unused_ports,
    handle_call({create_port, OwnerID, Port}, From, State);

handle_call({create_port, OwnerID, Port}, _From, State) ->
    #state{unused_ports=UnusedPorts, host=Host} = State,
    {Reply, State1} = case lists:member(Port, UnusedPorts) of
        true ->
            NewUnusedPorts = UnusedPorts -- [Port],
            {ok, _, _} = oscilloscope_metadata:named(
                insert_port,
                [OwnerID, Host, Port]
            ),
            oscilloscope_entities_port:start(OwnerID, Port),
            %TODO Add call to external stunnel-refreshing proc here
            {{ok, Port}, State#state{unused_ports=NewUnusedPorts}};
        false ->
            {{error, <<"port in use">>}, State}
    end,
    {reply, Reply, State1}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
