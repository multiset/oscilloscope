-module(osc_entities_port).

-export([start/2, create/1, create/2]).

-include("osc_entities.hrl").

-define(PORT_SERVER, osc_entities_port_server).


-spec start(integer(), integer()) -> ok | {error, binary()}.
start(OwnerID, Port) ->
    {ok, Pid} = ranch:start_listener(
        OwnerID,
        1,
        ranch_tcp,
        [{port, Port}],
        osc_net_tcp,
        [{parser, fun osc_net_protocols:graphite/1}]
    ),
    {ok, Pid}.


-spec create(#org{} | #user{}) -> {ok, integer()} | {error, binary()}.
create(User) when is_record(User, user) ->
    create_int(User#user.owner_id);
create(Org) when is_record(Org, org) ->
    create_int(Org#org.owner_id).

create_int(OwnerID) when is_integer(OwnerID) ->
    gen_server:call(?PORT_SERVER, {create_port, OwnerID}).


-spec create(#org{} | #user{}, integer()) -> {ok, integer()} | {error, binary()}.
create(User, Port) when is_record(User, user) ->
    create_int(User#user.owner_id, Port);
create(Org, Port) when is_record(Org, org) ->
    create_int(Org#org.owner_id, Port).

create_int(OwnerID, Port) when is_integer(OwnerID), is_integer(Port) ->
    gen_server:call(?PORT_SERVER, {create_port, OwnerID, Port}).
