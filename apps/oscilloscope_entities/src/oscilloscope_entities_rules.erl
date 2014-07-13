-module(oscilloscope_entities_rules).

-export([
    grant/4
]).

grant(OrgID, TeamID, Tags, Level) ->
    gen_server:call(
        oscilloscope_entities_server,
        {grant_perms, OrgID, TeamID, Tags, Level}
    ).
