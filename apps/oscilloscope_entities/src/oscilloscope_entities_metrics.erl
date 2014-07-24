-module(oscilloscope_entities_metrics).

-export([
    find/3
]).

-include_lib("oscilloscope_entities/include/oscilloscope_entities.hrl").

-spec find(#org{}, #user{}, [{binary(), binary()}]) -> any().
find(Org, User, Tags) ->
    gen_server:call(
        oscilloscope_entities_server,
        {find_metrics, Org#org.id, User#user.id, Tags}
    ).
