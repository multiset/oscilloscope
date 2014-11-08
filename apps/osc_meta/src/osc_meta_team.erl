-module(osc_meta_team).

-export([
    lookup/1,
    lookup_members/1,
    create/2,
    delete/2,
    members/2,
    add_member/3,
    remove_member/3,
    is_member/3
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
            {ok, [
                {id, TeamID},
                {name, TeamName},
                {orgid, OrgID},
                {members, Members}
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

-spec create(org_id(), binary()) -> {ok, team_id()}.
create(OrgID, Name) ->
    {ok, 1, _, [{TeamID}]} = osc_sql:named(create_team, [OrgID, Name]),
    {ok, TeamID}.

-spec delete(org_id(), team_id()) -> ok.
delete(OrgID, TeamID) ->
    osc_sql:batch([
        {delete_team, [OrgID, TeamID]},
        {delete_team_members, [OrgID, TeamID]}
    ]),
    ok.

-spec members(org_id(), team_id()) -> [user_id()].
members(OrgID, TeamID) ->
    {ok, _, Members} = osc_sql:named(get_team_members, [OrgID, TeamID]),
    lists:map(fun({{UserID}}) -> UserID end, Members).

-spec add_member(org_id(), team_id(), user_id()) -> ok.
add_member(OrgID, TeamID, UserID) ->
    osc_sql:named(add_team_member, [OrgID, TeamID, UserID]),
    ok.

-spec remove_member(org_id(), team_id(), user_id()) -> ok.
remove_member(OrgID, TeamID, UserID) ->
    osc_sql:named(remove_team_member, [OrgID, TeamID, UserID]),
    ok.

-spec is_member(org_id(), team_id(), user_id()) -> boolean().
is_member(OrgID, TeamID, UserID) ->
    {ok, _, [{IsMember}]} = osc_sql:named(is_team_member, [OrgID, TeamID, UserID]),
    IsMember.
