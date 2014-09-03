-module(osc_entities_rules).

-export([
    grant/4
]).

-include_lib("osc_entities/include/osc_entities.hrl").

-spec grant(#org{}, #team{}, [{binary(), binary()}], integer()) -> ok.
grant(Org, Team, Tags, Level) ->
    gen_server:call(
        osc_entities_server,
        {grant_perms, Org#org.id, Team#team.id, Tags, Level}
    ).
