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
    is_member/2
]).

-include_lib("osc/include/osc_types.hrl").

-spec lookup(org_id() | binary()) -> {ok, meta()} | not_found.
lookup(NameOrID) ->
    {ok, Org} = if is_binary(NameOrID) ->
        {ok, _, Org0} = osc_sql:named(get_org_by_name, [NameOrID]),
        {ok, Org0};
    true ->
        {ok, _, Org0} = osc_sql:named(get_org_by_id, [NameOrID]),
        {ok, Org0}
    end,
    case Org of
        [] ->
            not_found;
        [{ID, OwnerID, Name, _Active}] ->
            {ok, [{id, ID}, {owner_id, OwnerID}, {name, Name}]}
    end.

-spec create(binary(), user_id()) -> {ok, org_id()}.
create(OrgName, UserID) ->
    [{ok, 1, _, [{OrgID}]}, {ok,1}, {ok,1}] = osc_sql:batch([
        {create_org, [UserID, OrgName]},
        {add_org_member_by_org_name, [UserID, OrgName]},
        {add_owner_by_org_name, [UserID, OrgName]}
    ]),
    {ok, OrgID}.

-spec delete(user_id()) -> ok.
delete(OrgID) ->
    [{ok, _}, {ok, _}, {ok, _}, {ok, _}] = osc_sql:batch([
        {delete_org_team_members, [OrgID]},
        {delete_org_members, [OrgID]},
        {delete_teams, [OrgID]},
        {delete_org, [OrgID]}
    ]),
    ok.

-spec teams(user_id()) -> [].
teams(OrgID) ->
    SQL = <<
        "SELECT teams.id, teams.name,"
        " (SELECT COUNT(*) FROM team_members WHERE team_id = teams.id)"
        " FROM teams"
        " WHERE teams.org_id = $1"
    >>,
    {ok, _, Teams} = osc_sql:adhoc(SQL, [OrgID]),
    Teams.

-spec members(org_id()) -> [binary()].
members(OrgID) ->
    {ok, _, Members} = osc_sql:named(get_org_members, [OrgID]),
    lists:map(fun({UserID}) -> UserID end, Members).

-spec is_owner(org_id(), user_id()) -> boolean().
is_owner(OrgID, UserID) ->
    {ok, _, [{IsOwner}]} = osc_sql:named(is_org_owner, [UserID, OrgID]),
    IsOwner.

-spec add_member(org_id(), user_id()) -> ok.
add_member(OrgID, UserID) ->
    {ok, 1} = osc_sql:named(add_org_member, [OrgID, UserID]),
    ok.

-spec remove_member(org_id(), user_id()) -> ok.
remove_member(OrgID, UserID) ->
    [{ok,1},{ok,1}] = osc_sql:batch([
        {remove_org_member, [OrgID, UserID]},
        {remove_org_member_from_teams, [OrgID, UserID]}
    ]),
    ok.

-spec is_member(org_id(), user_id()) -> boolean().
is_member(OrgID, UserID) ->
    {ok, _, [{IsMember}]} = osc_sql:named(is_org_member, [OrgID, UserID]),
    IsMember.
