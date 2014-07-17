-module(oscilloscope_auth_org).

-export([
    can_delete/2,
    can_add_member/2,
    can_remove_member/3,
    can_add_port/2
]).

-include_lib("oscilloscope_entities/include/oscilloscope_entities.hrl").

-spec can_delete(#user{}, #org{}) -> boolean().
can_delete(AuthedUser, Org) ->
    oscilloscope_auth_org:is_owner(Org, AuthedUser).

-spec can_add_member(#user{}, #org{}) -> boolean().
can_add_member(AuthedUser, Org) ->
    oscilloscope_entities_org:is_owner(Org, AuthedUser).

-spec can_remove_member(#user{}, #org{}, #user{}) -> boolean().
can_remove_member(AuthedUser, Org, User) ->
    IsSelf = AuthedUser#user.id =:= User#user.id,
    IsSelf orelse oscilloscope_entities_org:is_owner(Org, AuthedUser).

-spec can_add_port(#user{}, #org{}) ->boolean().
can_add_port(AuthedUser, Org) ->
    oscilloscope_entities_org:is_owner(Org, AuthedUser).
