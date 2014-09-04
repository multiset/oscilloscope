-module(osc_entities_metrics).

-export([
    find/3
]).

-include_lib("osc_entities/include/osc_entities.hrl").

-spec find(#org{}, #user{}, [{binary(), binary()}]) -> any().
find(Org, User, Tags) ->
    gen_server:call(
        osc_entities_server,
        {find_metrics, Org#org.id, User#user.id, Tags}
    ).
