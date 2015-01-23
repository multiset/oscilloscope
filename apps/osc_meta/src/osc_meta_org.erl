-module(osc_meta_org).

-export([
    lookup/1,
    create/2,
    delete/1,
    teams/1,
    members/1,
    is_owner/2,
    add_member/2,
    remove_member/2,
    is_member/2,
    team_permissions/2,
    add_port/2,
    remove_port/2
]).

-include_lib("osc/include/osc_types.hrl").
-include_lib("osc_meta/include/osc_meta.hrl").

-spec lookup(NameOrID) -> {ok, Props} | not_found when
    NameOrID :: binary() | org_id(),
    Props :: proplists:proplist().

lookup(Name) when is_binary(Name) ->
    SQL = "SELECT id FROM orgs WHERE name = $1;",
    {ok, _, Org} = mpgsql:equery(SQL, [Name]),
    case Org of
        [] ->
            not_found;
        [{OrgID}] ->
            {ok, [
                {id, OrgID},
                {name, Name},
                {ports, ports(OrgID)}
            ]}
    end;
lookup(OrgID) ->
    SQL = "SELECT name FROM orgs WHERE id = $1;",
    {ok, _, Org} = mpgsql:equery(SQL, [OrgID]),
    case Org of
        [] ->
            not_found;
        [{Name}] ->
            {ok, [
                {id, OrgID},
                {name, Name},
                {ports, ports(OrgID)}
            ]}
    end.


-spec ports(OrgID) -> Ports when
    OrgID :: org_id(),
    Ports :: [pos_integer()].

ports(OrgID) ->
    {ok, _, Ports} = mpgsql:equery(
        "SELECT port FROM ports WHERE org_id=$1",
        [OrgID]
    ),
    [Port || {Port} <- Ports].


-spec create(Name, UserID) -> {ok, OrgID} | {error, Error} when
    Name :: binary(),
    UserID :: user_id(),
    OrgID :: org_id(),
    Error :: exists.

create(OrgName, UserID) ->
    ok = mpgsql:tx_begin(),
    CreateOrgSQL = "WITH org AS ("
                   "  INSERT INTO orgs (name) "
                   "  VALUES ($1) RETURNING id "
                   ") "
                   "INSERT INTO teams (name, org_id) "
                   "SELECT 'owners', id FROM org RETURNING org_id;",
    AddMemberSQL = "INSERT INTO org_members (org_id, user_id) VALUES ($1, $2);",
    AddOwnerSQL = "INSERT INTO team_members (team_id, user_id) "
                  "SELECT id, $2 FROM TEAMS "
                  "WHERE name='owners' AND org_id = $1;",

    case mpgsql:equery(CreateOrgSQL, [OrgName]) of
        {error, unique_violation} ->
            ok = mpgsql:tx_rollback(),
            {error, exists};
        {ok, 1, _, [{OrgID}]} ->
            {ok, 1} = mpgsql:equery(AddMemberSQL, [OrgID, UserID]),
            {ok, 1} = mpgsql:equery(AddOwnerSQL, [OrgID, UserID]),
            ok = mpgsql:tx_commit(),
            {ok, OrgID}
    end.


-spec delete(org_id()) -> ok.

delete(OrgID) ->
    ok = mpgsql:tx_begin(),
    DeleteOrgTeamMembersSQL = "DELETE FROM team_members WHERE team_id= "
                              "(SELECT id FROM teams WHERE org_id = $1);",
    DeleteOrgMembersSQL = "DELETE FROM org_members WHERE org_id = $1;",
    DeleteTeamsSQL = "DELETE FROM teams WHERE org_id = $1;",
    DeleteOrgSQL = "DELETE FROM orgs WHERE id = $1",
    {ok, _} = mpgsql:equery(DeleteOrgTeamMembersSQL, [OrgID]),
    {ok, _} = mpgsql:equery(DeleteOrgMembersSQL, [OrgID]),
    {ok, _} = mpgsql:equery(DeleteTeamsSQL, [OrgID]),
    {ok, _} = mpgsql:equery(DeleteOrgSQL, [OrgID]),
    ok = mpgsql:tx_commit(),
    ok.


-spec teams(OrgID) -> Teams when
    OrgID :: org_id(),
    Teams :: [{TeamID, TeamName, TeamMembers}],
    TeamID :: team_id(),
    TeamName :: binary(),
    TeamMembers :: non_neg_integer().

teams(OrgID) ->
    SQL = "SELECT teams.id, teams.name, "
          "(SELECT COUNT(*) FROM team_members WHERE team_id = teams.id) "
          "FROM teams WHERE teams.org_id = $1;",
    {ok, _, Teams} = mpgsql:equery(SQL, [OrgID]),
    Teams.


-spec members(OrgID) -> Members when
    OrgID :: org_id(),
    Members :: [user_id()].

members(OrgID) ->
    SQL = "SELECT user_id FROM org_members WHERE org_id = $1;",
    {ok, _, Members} = mpgsql:equery(SQL, [OrgID]),
    lists:map(fun({UserID}) -> UserID end, Members).


-spec is_owner(org_id(), user_id()) -> boolean().

is_owner(OrgID, UserID) ->
    SQL = "SELECT EXISTS ( "
          "  SELECT 1 FROM team_members WHERE team_id=( "
          "    SELECT id FROM teams WHERE org_id = $1 AND name='owners' "
          "  ) AND user_id = $2 "
          ");",
    {ok, _, [{IsOwner}]} = mpgsql:equery(SQL, [OrgID, UserID]),
    IsOwner.


-spec add_member(org_id(), user_id()) -> ok.

add_member(OrgID, UserID) ->
    SQL = "INSERT INTO org_members (org_id, user_id) VALUES ($1, $2);",
    {ok, 1} = mpgsql:equery(SQL, [OrgID, UserID]),
    ok.


-spec remove_member(org_id(), user_id()) -> ok.

remove_member(OrgID, UserID) ->
    ok = mpgsql:tx_begin(),
    RemoveFromTeamsSQL = "DELETE FROM team_members WHERE team_id=( "
                         "  SELECT id FROM teams WHERE org_id = $1 "
                         ") AND user_id = $2;",
    RemoveFromOrgSQL = "DELETE FROM org_members "
                       "WHERE org_id = $1 AND user_id = $2;",
    {ok, _} = mpgsql:equery(RemoveFromTeamsSQL, [OrgID, UserID]),
    {ok, _} = mpgsql:equery(RemoveFromOrgSQL, [OrgID, UserID]),
    ok = mpgsql:tx_commit(),
    ok.


-spec is_member(org_id(), user_id()) -> boolean().

is_member(OrgID, UserID) ->
    SQL = "SELECT EXISTS ( "
          "  SELECT 1 FROM org_members WHERE org_id = $1 AND user_id = $2 "
          ");",
    {ok, _, [{IsMember}]} = mpgsql:equery(SQL, [OrgID, UserID]),
    IsMember.


-spec team_permissions(org_id(), user_id()) -> [{any(), any()}].

team_permissions(OrgID, UserID) ->
    SQL = "SELECT tags FROM team_permissions WHERE team_id IN ( "
          "  SELECT id FROM teams "
          "  WHERE org_id = $1 "
          "  AND id IN ( "
          "    SELECT team_id FROM team_members "
          "    WHERE user_id = $2 "
          "  ) "
          ");",
    {ok, _, Rows} = mpgsql:equery(SQL, [OrgID, UserID]),
    lists:map(
        fun({Permissions}) -> lists:map(fun list_to_tuple/1, Permissions) end,
        Rows
    ).


-spec add_port(org_id(), pos_integer()) -> ok.

add_port(OrgID, Port) ->
    Type = <<"graphite">>,
    SQL = "INSERT INTO ports (org_id, port, type) VALUES ($1, $2, $3);",
    {ok, 1} = mpgsql:equery(SQL, [OrgID, Port, Type]),
    ok.


-spec remove_port(org_id(), pos_integer()) -> ok.

remove_port(OrgID, Port) ->
    SQL = "DELETE FROM ports WHERE org_id=$1 AND port=$2",
    {ok, 1} = mpgsql:equery(SQL, [OrgID, Port]),
    ok.
