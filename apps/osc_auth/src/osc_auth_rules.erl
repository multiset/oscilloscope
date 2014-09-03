-module(osc_auth_rules).

-export([
    can_grant/2
]).

-include_lib("osc_entities/include/osc_entities.hrl").

-spec can_grant(#user{}, #org{}) -> boolean().
can_grant(AuthedUser, Org) ->
    osc_entities_org:is_owner(AuthedUser, Org).
