-module(osc_auth_org).

-export([
    can_delete/2,
    can_add_member/2,
    can_remove_member/3,
    can_add_port/2
]).

-include_lib("osc_entities/include/osc_entities.hrl").

-spec can_delete(#user{}, #org{}) -> boolean().
can_delete(AuthedUser, Org) ->
    osc_auth_org:is_owner(Org, AuthedUser).

-spec can_add_member(#user{}, #org{}) -> boolean().
can_add_member(AuthedUser, Org) ->
    osc_entities_org:is_owner(Org, AuthedUser).

-spec can_remove_member(#user{}, #org{}, #user{}) -> boolean().
can_remove_member(AuthedUser, Org, User) ->
    IsSelf = AuthedUser#user.id =:= User#user.id,
    IsSelf orelse osc_entities_org:is_owner(Org, AuthedUser).

-spec can_add_port(#user{}, #org{}) ->boolean().
can_add_port(AuthedUser, Org) ->
    osc_entities_org:is_owner(Org, AuthedUser).
