-module(oscilloscope_auth_rules).

-export([
    can_grant/2
]).

-include_lib("oscilloscope_entities/include/oscilloscope_entities.hrl").

-spec can_grant(#user{}, #org{}) -> boolean().
can_grant(AuthedUser, Org) ->
    oscilloscope_entities_org:is_owner(AuthedUser, Org).
