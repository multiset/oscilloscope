-module(oscilloscope_auth_metrics).

-export([
    can_find/2
]).

-include_lib("oscilloscope_entities/include/oscilloscope_entities.hrl").

-spec can_find(#user{}, #org{}) -> boolean().
can_find(AuthedUser, Org) ->
    oscilloscope_entities_org:is_member(AuthedUser, Org).
