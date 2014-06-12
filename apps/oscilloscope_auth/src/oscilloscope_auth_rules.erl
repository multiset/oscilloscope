-module(oscilloscope_auth_rules).

-export([
    grant/4
]).

grant(OrgID, TeamID, Tags, Level) ->
    gen_server:call(
        oscilloscope_auth_server,
        {grant_perms, OrgID, TeamID, Tags, Level}
    ).
