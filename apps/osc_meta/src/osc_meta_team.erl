-module(osc_meta_team).

-export([
    lookup/1,
    lookup_members/1,
    create/2,
    delete/1,
    members/1,
    add_member/2,
    remove_member/2,
    is_member/2,
    remove_permission/2,
    add_permission/2
]).

-include_lib("osc/include/osc_types.hrl").

-spec lookup(team_id()) -> not_found | {ok, proplists:proplist()}.

lookup(TeamID) ->
    SQL = "SELECT teams.name, teams.org_id FROM teams WHERE teams.id = $1;",
    {ok, _Schema, Rows} = mpgsql:equery(SQL, [TeamID]),
    case Rows of
        [] ->
            not_found;
        [{TeamName, OrgID}] ->
            {ok, Members} = lookup_members(TeamID),
            {ok, Permissions} = lookup_permissions(TeamID),
            {ok, [
                {id, TeamID},
                {name, TeamName},
                {orgid, OrgID},
                {members, Members},
                {permissions, Permissions}
            ]}
    end.

-spec lookup_members(TeamID) -> {ok, Members} when
    TeamID :: team_id(),
    Members :: [{UserID, Username}],
    UserID :: user_id(),
    Username :: binary().

lookup_members(TeamID) ->
    SQL = "SELECT users.id, users.name FROM users "
          "JOIN team_members on users.id=team_members.user_id "
          "WHERE team_members.team_id = $1;",
    {ok, _Schema, Rows} = mpgsql:equery(SQL, [TeamID]),
    {ok, Rows}.


-spec lookup_permissions(TeamID) -> {ok, Permissions} when
    TeamID :: team_id(),
    Permissions :: proplists:proplist().

lookup_permissions(TeamID) ->
    SQL = "SELECT id, tags FROM team_permissions WHERE team_id = $1;",
    {ok, _Schema, Rows} = mpgsql:equery(SQL, [TeamID]),
    Perms = lists:map(
        fun({ID, Tags}) -> {ID, lists:map(fun list_to_tuple/1, Tags)} end,
        Rows
    ),
    {ok, Perms}.

-spec create(org_id(), binary()) -> {ok, team_id()}.

create(OrgID, Name) ->
    SQL = "INSERT INTO teams (org_id, name) VALUES ($1, $2) RETURNING id;",
    {ok, 1, _, [{TeamID}]} = mpgsql:equery(SQL, [OrgID, Name]),
    mstat:increment_counter([osc_meta, creations, team]),
    {ok, TeamID}.


-spec delete(team_id()) -> ok.

delete(TeamID) ->
    DeleteMembersSQL = "DELETE FROM team_members WHERE team_id = $1;",
    DeleteTeamSQL = "DELETE FROM teams WHERE id = $1;",
    ok = mpgsql:tx_begin(),
    {ok, _} = mpgsql:equery(DeleteMembersSQL, [TeamID]),
    {ok, _} = mpgsql:equery(DeleteTeamSQL, [TeamID]),
    ok = mpgsql:tx_commit(),
    ok.

-spec members(team_id()) -> [user_id()].

members(TeamID) ->
    SQL = "SELECT user_id FROM team_members WHERE team_id = $1;",
    {ok, _, Members} = mpgsql:equery(SQL, [TeamID]),
    lists:map(fun({{UserID}}) -> UserID end, Members).

-spec add_member(team_id(), user_id()) -> ok.

add_member(TeamID, UserID) ->
    SQL = "INSERT INTO team_members (team_id, user_id) VALUES ($1, $2);",
    {ok, 1} = mpgsql:equery(SQL, [TeamID, UserID]),
    ok.

-spec remove_member(team_id(), user_id()) -> ok.

remove_member(TeamID, UserID) ->
    SQL = "DELETE FROM team_members WHERE team_id = $1 AND user_id = $2;",
    {ok, 1} = mpgsql:equery(SQL, [TeamID, UserID]),
    ok.

-spec is_member(team_id(), user_id()) -> boolean().

is_member(TeamID, UserID) ->
    SQL = "SELECT EXISTS"
          "(SELECT 1 FROM team_members WHERE team_id = $1 AND user_id = $2);",
    {ok, _, [{IsMember}]} = mpgsql:equery(SQL, [TeamID, UserID]),
    IsMember.


-spec remove_permission(team_id(), any()) -> ok.

remove_permission(TeamID, PermissionID) ->
    SQL = "DELETE FROM team_permissions WHERE team_id = $1 AND id = $2",
    {ok, 1} = mpgsql:equery(SQL, [TeamID, PermissionID]),
    ok.


-spec add_permission(team_id(), [{any(), any()}]) -> {ok, any()}.

add_permission(TeamID, PermissionProps) ->
    SQL = " INSERT INTO team_permissions (team_id, tags)"
          " VALUES ($1, $2) RETURNING id;",
    {ok, _, _, [{ID}]} = mpgsql:equery(
        SQL,
        [TeamID, lists:map(fun tuple_to_list/1, PermissionProps)]
    ),
    {ok, ID}.
