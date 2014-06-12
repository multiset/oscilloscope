-module(oscilloscope_auth_metrics).

-export([
    get_metrics/3
]).

get_metrics(OrgID, UserID, Tags) ->
    gen_server:call(
        oscilloscope_auth_server,
        {get_metrics, OrgID, UserID, Tags}
    ).
