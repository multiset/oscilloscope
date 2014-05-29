-module(oscilloscope_auth_team).

-export([create/2, get_id/1]).

-include("oscilloscope_auth.hrl").


-spec create(integer(), binary()) -> {ok, tuple()}.
create(OrgID, Name) ->
    {ok, 1} = oscilloscope_metadata:named(create_team, [Name, OrgID]),
    {ok, _, [{Id}]} = oscilloscope_metadata:named(
        select_team_id,
        [OrgID, Name]
    ),
    {ok, #team{name=Name, id=Id, org_id=OrgID}}.

-spec get_id(binary()) -> {ok, integer()} | not_found.
get_id(Name) ->
    oscilloscope_metadata:named(get_team_id, [Name]).
