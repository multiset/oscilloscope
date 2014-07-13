-module(oscilloscope_entities_metrics).

-export([
    find/3
]).

find(OrgID, UserID, Tags) ->
    gen_server:call(
        oscilloscope_entities_server,
        {find_metrics, OrgID, UserID, Tags}
    ).
