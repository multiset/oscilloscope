-module(osc_meta_org).

-export([
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

-spec create(owner_id(), binary()) -> {ok, org_id()}.
create(OwnerID, Name) ->
    {ok, _, _, [{OrgID}]} = osc_sql:named(create_org, [OwnerID, Name]),
    {ok, OrgID}.

-spec delete(org_id()) -> ok.
delete(OrgID) ->
    {ok, _, _} = osc_sql:named(delete_org, [OrgID]),
    ok.

-spec teams(org_id()) -> [team_id()].
teams(OrgID) ->
    {ok, _, _, Teams} = osc_sql:named(get_org_teams, [OrgID]),
    Teams.

-spec members(org_id()) -> [user_id()].
members(OrgID) ->
    {ok, _, _, Members} = osc_sql:named(get_org_teams, [OrgID]),
    Members.

-spec is_owner(user_id(), org_id()) -> boolean().
is_owner(UserID, OrgID) ->
    {ok, _, _, IsOwner} = osc_sql:named(is_org_owner, [UserID, OrgID]),
    IsOwner.

-spec add_member(org_id(), user_id()) -> ok.
add_member(OrgID, UserID) ->
    {ok, 1} = osc_sql:named(add_org_member, [OrgID, UserID]),
    ok.

-spec remove_member(org_id(), user_id()) -> ok.
remove_member(OrgID, UserID) ->
    {ok, 1} = osc_sql:named(remove_org_member, [OrgID, UserID]),
    ok.

-spec is_member(org_id(), user_id()) -> boolean().
is_member(OrgID, UserID) ->
    {ok, _, _, IsMember} = osc_sql:named(is_org_member, [OrgID, UserID]),
    IsMember.
