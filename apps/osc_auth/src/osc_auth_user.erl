-module(osc_auth_user).

-export([is_authorized/2, can_delete/2, can_add_port/2]).

-include_lib("osc_entities/include/osc_entities.hrl").

-spec is_authorized(binary(), binary()) -> boolean().
is_authorized(Name, Pass) ->
    case osc_entities_user:lookup(Name) of
        {ok, User} ->
            {ok, LHash} = bcrypt:hashpw(Pass, User#user.password),
            User#user.password =:= list_to_binary(LHash);
        _ ->
            false
    end.

-spec can_delete(#user{}, #user{}) -> boolean().
can_delete(AuthedUser, User) ->
    AuthedUser#user.id =:= User#user.id.

-spec can_add_port(#user{}, #user{}) -> {ok, integer()} | unauthorized.
can_add_port(AuthedUser, User) ->
    AuthedUser#user.id =:= User#user.id.
