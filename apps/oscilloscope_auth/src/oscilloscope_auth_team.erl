-module(oscilloscope_auth_team).

-export([create/2, get_team/2, add_members/3, remove_members/3, is_member/3]).

-include("oscilloscope_auth.hrl").


-spec create(integer(), binary()) -> {ok, tuple()}.
create(OrgID, Name) ->
    {ok, 1} = oscilloscope_metadata_sql:named(create_team, [Name, OrgID]),
    {ok, _, [{Id}]} = oscilloscope_metadata_sql:named(
        select_team_id,
        [OrgID, Name]
    ),
    {ok, #team{name=Name, id=Id, org_id=OrgID}}.

-spec get_team(binary() | #org{} | integer(), binary() | #team{} | integer()) -> {ok, integer()} | not_found.
get_team(Org, Team) ->
    case oscilloscope_auth_org:get_org(Org) of
        {ok, Org} ->
            get_team_int(Org#org.id, Team);
        not_found ->
            not_found
    end.

get_team_int(OrgID, Name) when is_binary(Name) ->
    case ets:lookup(team_ids, {OrgID, Name}) of
        [] ->
            not_found;
        [{TeamID}] ->
            #team{name=Name, id=TeamID}
    end;
get_team_int(_OrgID, ID) when is_integer(ID) ->
    {ok, ID}.


-spec add_members(integer() | #org{} | binary(), integer() | #team{} | binary(), integer()) -> ok.
add_members(Org, Team, Users) ->
    {TeamID, UserIDs} = get_team_and_user_ids(Org, Team, Users),
    {ok, _, _} = oscilloscope_metadata_sql:named(
        add_users_to_team,
        [TeamID|UserIDs]
    ),
    ok.

-spec remove_members(integer() | #org{} | binary(), integer() | #team{} | binary(), integer()) -> ok.
remove_members(Org, Team, Users) ->
    {TeamID, UserIDs} = get_team_and_user_ids(Org, Team, Users),
    {ok, _, _} = oscilloscope_metadata_sql:named(
        remove_users_from_team,
        [TeamID|UserIDs]
    ),
    ok.

get_team_and_user_ids(Org0, Team0, Users0) ->
    Org = oscilloscope_auth_org:get_org(Org0),
    Team = get_team(Org, Team0),
    Users = [oscilloscope_auth_user:get_user(User) || User <- Users0],
    {Team#team.id, [User#user.id || User <- Users]}.

-spec is_member(#user{} | integer(), #team{} | integer(), #org{} | integer()) -> boolean().
is_member(#user{}=User, Team, Org) ->
    is_member(User#user.id, Team, Org);
is_member(User, #team{}=Team, Org) ->
    is_member(User, Team#team.id, Org);
is_member(User, Team, #org{}=Org) ->
    is_member(User, Team, Org#org.id);
is_member(UserID, TeamID, OrgID) ->
    ets:member(otu, {OrgID, TeamID, UserID}).
