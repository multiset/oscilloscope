-module(osc_meta_team).

-export([
    create/2,
    delete/2,
    members/2,
    add_member/3,
    remove_member/3,
    is_member/3
]).

-include_lib("osc/include/osc_types.hrl").

-spec create(org_id(), binary()) -> ok.
create(OrgID, Name) ->
    osc_sql:named(create_team, [OrgID, Name]),
    ok.

-spec delete(org_id(), team_id()) -> ok.
delete(OrgID, TeamID) ->
    osc_sql:named(delete_team, [OrgID, TeamID]),
    ok.

-spec members(org_id(), team_id()) -> [user_id()].
members(OrgID, TeamID) ->
    {ok, _, Members} = osc_sql:named(get_team_members, [OrgID, TeamID]),
    Members.

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
