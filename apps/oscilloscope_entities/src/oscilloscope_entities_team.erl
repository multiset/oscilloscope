-module(oscilloscope_entities_team).

-export([
    create/2,
    delete/2,
    lookup/2,
    members/2,
    add_member/3,
    remove_member/3,
    is_member/3
]).

-include("oscilloscope_entities.hrl").


-spec create(#org{}, binary()) -> {ok, tuple()}.
create(Org, Name) ->
    #org{id=OrgID} = Org,
    {ok, 1, _, [{ID}]} = oscilloscope_metadata_sql:named(insert_team, [Name, OrgID]),
    Team = #team{name=Name, id=ID, org_id=OrgID},
    true = ets:insert(teams, Team),
    {ok, Team}.

-spec delete(#org{}, #team{}) -> ok.
delete(Org, Team) ->
    #org{id=OrgID} = Org,
    #team{id=TeamID, name=TeamName} = Team,
    {ok, _} = oscilloscope_metadata_sql:named(delete_team, [OrgID, TeamID]),
    true = ets:delete(teams, TeamName),
    true = ets:match_delete(team_members, {OrgID, TeamID, '_'}),
    ok.

-spec lookup(#org{}, binary()) -> {ok, integer()} | not_found.
lookup(Org, TeamName) ->
    case ets:lookup(teams, {Org#org.id, TeamName}) of
        [] ->
            not_found;
        [#team{}=Team] ->
            Team
    end.

-spec members(#org{}, #team{}) -> [#user{}].
members(Org, Team) ->
    UserIDs = ets:match(team_members, {Org#org.id, Team#team.id, '$1'}),
    lists:foldl(fun([UserID], Acc) ->
        case ets:lookup(users, UserID) of
            [#user{}=User] ->
                [User|Acc];
            [] ->
                lager:error(
                    "UserID ~p in team_members ets table but not users table",
                    [UserID]
                ),
                Acc
        end
    end, [], UserIDs).


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
