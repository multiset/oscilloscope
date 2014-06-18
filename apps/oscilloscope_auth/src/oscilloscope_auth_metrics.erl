-module(oscilloscope_auth_metrics).

-export([
    get/3
]).

get(OrgID, UserID, Tags) ->
    gen_server:call(
        oscilloscope_auth_server,
        {get_metrics, OrgID, UserID, Tags}
    ).
