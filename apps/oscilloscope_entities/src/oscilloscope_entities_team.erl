-module(oscilloscope_entities_team).

-export([create/2, delete/2, lookup/2, add_member/3, remove_member/3, is_member/3]).

-include("oscilloscope_entities.hrl").


-spec create(#org{}, binary()) -> {ok, tuple()}.
create(Org, Name) ->
    #org{id=OrgID} = Org,
    {ok, 1, _, [{ID}]} = oscilloscope_metadata_sql:named(insert_team, [Name, OrgID]),
    {ok, #team{name=Name, id=ID, org_id=OrgID}}.

-spec delete(#org{}, #team{}) -> ok.
delete(Org, Team) ->
    #org{id=OrgID} = Org,
    #team{id=TeamID} = Team,
    {ok, _} = oscilloscope_metadata_sql:named(delete_team, [OrgID, TeamID]),
    ok.

-spec lookup(#org{}, binary()) -> {ok, integer()} | not_found.
lookup(Org, TeamName) ->
    case ets:lookup(team_ids, {Org#org.id, TeamName}) of
        [] ->
            not_found;
        [{TeamID}] ->
            #team{name=TeamName, id=TeamID}
    end.

-spec add_member(#org{}, #team{}, #user{}) -> ok.
add_member(Org, Team, User) ->
    ets:insert(team_members, {Org#org.id, Team#team.id, User#user.id}),
    {ok, _} = oscilloscope_metadata_sql:named(
        add_user_to_team,
        [Org#org.id, Team#team.id, User#user.id]
    ),
    ok.

-spec remove_member(#org{}, #team{}, #user{}) -> ok.
remove_member(Org, Team, User) ->
    ets:delete(team_members, {Org#org.id, Team#team.id, User#user.id}),
    {ok, _, _} = oscilloscope_metadata_sql:named(
        remove_user_from_team,
        [Team#team.id, User#user.id]
    ),
    ok.

-spec is_member(#org{}, #team{}, #user{}) -> boolean().
is_member(Org, Team, User) ->
    ets:member(team_members, {Org#org.id, Team#team.id, User#user.id}).
