-module(osc_auth_team).

-export([can_create/2, can_delete/2, can_add_member/2, can_remove_member/3]).

-include_lib("osc_entities/include/osc_entities.hrl").

-spec can_create(#user{}, #org{}) -> boolean().
can_create(AuthedUser, Org) ->
    osc_entities_org:is_owner(AuthedUser, Org).

-spec can_delete(#user{}, #org{}) -> boolean().
can_delete(AuthedUser, Org) ->
    osc_auth_org:is_owner(AuthedUser, Org).

-spec can_add_member(#user{}, #org{}) -> boolean().
can_add_member(AuthedUser, Org) ->
    osc_auth_org:is_owner(AuthedUser, Org).

-spec can_remove_member(#user{}, #org{}, #user{}) -> boolean().
can_remove_member(AuthedUser, Org, User) ->
    IntegerID = is_integer(AuthedUser#user.id),
    AuthedAsSelf = IntegerID andalso AuthedUser#user.id =:= User#user.id,
    AuthedAsSelf orelse osc_entities_org:is_owner(AuthedUser, Org).
