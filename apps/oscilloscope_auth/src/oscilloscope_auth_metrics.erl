-module(oscilloscope_auth_metrics).

-export([
    find/3
]).

find(OrgID, UserID, Tags) ->
    gen_server:call(
        oscilloscope_auth_server,
        {find_metrics, OrgID, UserID, Tags}
    ).
