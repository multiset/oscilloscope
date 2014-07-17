-module(oscilloscope_auth_team).

-export([can_create/2, can_delete/2, can_add_member/2, can_remove_member/3]).

-include_lib("oscilloscope_entities/include/oscilloscope_entities.hrl").

-spec can_create(#user{}, #org{}) -> boolean().
can_create(AuthedUser, Org) ->
    oscilloscope_entities_org:is_owner(AuthedUser, Org).

-spec can_delete(#user{}, #org{}) -> boolean().
can_delete(AuthedUser, Org) ->
    oscilloscope_auth_org:is_owner(AuthedUser, Org).

-spec can_add_member(#user{}, #org{}) -> boolean().
can_add_member(AuthedUser, Org) ->
    oscilloscope_auth_org:is_owner(AuthedUser, Org).

-spec can_remove_member(#user{}, #org{}, #user{}) -> boolean().
can_remove_member(AuthedUser, Org, User) ->
    IntegerID = is_integer(AuthedUser#user.id),
    AuthedAsSelf = IntegerID andalso AuthedUser#user.id =:= User#user.id,
    AuthedAsSelf orelse oscilloscope_entities_org:is_owner(AuthedUser, Org).
