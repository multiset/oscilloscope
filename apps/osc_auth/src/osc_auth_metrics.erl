-module(osc_auth_metrics).

-export([
    can_find/2
]).

-include_lib("osc_entities/include/osc_entities.hrl").

-spec can_find(#user{}, #org{}) -> boolean().
can_find(AuthedUser, Org) ->
    osc_entities_org:is_member(AuthedUser, Org).
