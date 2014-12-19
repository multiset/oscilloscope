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

-spec lookup(team_id()) -> not_found | {ok, list()}.
lookup(TeamID) ->
    SQL = <<
        "SELECT teams.name, teams.org_id"
        " FROM teams"
        " WHERE teams.id=$1"
    >>,
    {ok, _Schema, Rows} = osc_sql:adhoc(SQL, [TeamID]),
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

-spec lookup_members(team_id()) -> {ok, list()}.
lookup_members(TeamID) ->
    SQL = <<
        "SELECT users.id, users.name"
        " FROM users"
        " JOIN team_members on users.id=team_members.user_id"
        " WHERE team_members.team_id=$1"
    >>,
    {ok, _Schema, Rows} = osc_sql:adhoc(SQL, [TeamID]),
    {ok, Rows}.

-spec lookup_permissions(team_id()) -> {ok, [any()]}.
lookup_permissions(TeamID) ->
    SQL = "SELECT id, tags FROM team_permissions WHERE team_id = $1",
    {ok, _Schema, Rows} = osc_sql:adhoc(SQL, [TeamID]),
    Perms = lists:map(
        fun({ID, Tags}) -> {ID, lists:map(fun list_to_tuple/1, Tags)} end,
        Rows
    ),
    {ok, Perms}.

-spec create(org_id(), binary()) -> {ok, team_id()}.
create(OrgID, Name) ->
    {ok, 1, _, [{TeamID}]} = osc_sql:named(create_team, [OrgID, Name]),
    {ok, TeamID}.

-spec delete(team_id()) -> ok | error.
delete(TeamID) ->
    Commands = [
        {delete_team_members, [TeamID]},
        {delete_team, [TeamID]}
    ],
    Batch = osc_sql:batch(Commands),
    case lists:usort([Status || {Status, _} <- Batch]) of
        [ok] ->
            ok;
        _ ->
            lager:error("Team delete SQL encountered an error: ~p", [Batch]),
            error
    end.

-spec members(team_id()) -> [user_id()].
members(TeamID) ->
    {ok, _, Members} = osc_sql:named(get_team_members, [TeamID]),
    lists:map(fun({{UserID}}) -> UserID end, Members).

-spec add_member(team_id(), user_id()) -> ok.
add_member(TeamID, UserID) ->
    osc_sql:named(add_team_member, [TeamID, UserID]),
    ok.

-spec remove_member(team_id(), user_id()) -> ok.
remove_member(TeamID, UserID) ->
    osc_sql:named(remove_team_member, [TeamID, UserID]),
    ok.

-spec is_member(team_id(), user_id()) -> boolean().
is_member(TeamID, UserID) ->
    {ok, _, [{IsMember}]} = osc_sql:named(is_team_member, [TeamID, UserID]),
    IsMember.


-spec remove_permission(team_id(), any()) -> ok.
remove_permission(TeamID, PermissionID) ->
    SQL = "DELETE FROM team_permissions WHERE team_id = $1 AND id = $2",
    {ok, 1} = osc_sql:adhoc(SQL, [TeamID, PermissionID]),
    ok.


-spec add_permission(team_id(), [{any(), any()}]) -> {ok, any()}.
add_permission(TeamID, PermissionProps) ->
    SQL = " INSERT INTO team_permissions (team_id, tags)"
          " VALUES ($1, $2) RETURNING id;",
    {ok, _, _, [{ID}]} = osc_sql:adhoc(
        SQL,
        [TeamID, lists:map(fun tuple_to_list/1, PermissionProps)]
    ),
    {ok, ID}.
