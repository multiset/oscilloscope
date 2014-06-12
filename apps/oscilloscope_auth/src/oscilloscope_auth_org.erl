-module(oscilloscope_auth_org).

-export([
    create/1,
    lookup/1,
    get_users/1,
    is_owner/2,
    add_members/2,
    remove_members/2,
    is_member/2
]).

-include("oscilloscope_auth.hrl").

-spec create(binary()) -> {ok, tuple()}.
create(Name) ->
    {ok, 1} = oscilloscope_metadata:named(create_org, [Name]),
    {ok, _, [{Id}]} = oscilloscope_metadata:named(select_org_id, [Name]),
    {ok, #org{name=Name, id=Id}}.

-spec lookup(binary() | #org{} | integer()) -> {ok, #org{}} | not_found.
lookup(Org) when is_record(Org, org) ->
    {ok, Org};
lookup(Name) when is_binary(Name) ->
    case ets:lookup(org_name_to_id, Name) of
        [] ->
            not_found;
        [{_, OrgID}] ->
            lookup(OrgID)
    end;
lookup(OrgID) when is_integer(OrgID) ->
    case ets:lookup(orgs, OrgID) of
        [] ->
            not_found;
        [#org{}=Org] ->
            {ok, Org}
    end.

get_users(OrgID) ->
    oscilloscope_metadata:named(get_users, [OrgID]).


-spec is_owner(#user{} | integer(), #org{} | integer()) -> boolean().
is_owner(User, #org{}=Org) ->
    is_owner(User, Org#org.id);
is_owner(User, OrgID) ->
    {_, TeamID} = ets:lookup(teams, {OrgID, <<"owners">>}),
    oscilloscope_auth_team:is_member(User, TeamID, OrgID).

-spec add_members(integer() | #org{} | binary(), integer()) -> ok.
add_members(Org0, Users0) ->
    Org = lookup(Org0),
    Users = [oscilloscope_auth_user:get_user(User) || User <- Users0],
    {ok, _, _} = oscilloscope_metadata:named(
        add_users_to_org,
        [Org#org.id|[User#user.id || User <- Users]]
    ),
    ok.

-spec remove_members(integer() | #org{} | binary(), integer()) -> ok.
remove_members(Org0, Users0) ->
    Org = lookup(Org0),
    Users = [oscilloscope_auth_user:get_user(User) || User <- Users0],

    lists:foldl(fun(User, _) ->
        TeamIDs = oscilloscope_auth_user:get_teams(User),
        {ok, _, _} = oscilloscope_metadata:named(
            remove_user_from_teams,
            [User#user.id|TeamIDs]
        )
    end, ok, Users),
    {ok, _, _} = oscilloscope_metadata:named(
        remove_users_from_org,
        [Org#org.id|[User#user.id || User <- Users]]
    ),

    ok.

-spec is_member(binary() | #org{} | integer(), binary() | #user{} | integer()) -> boolean().
is_member(Org0, User0) ->
    Org = lookup(Org0),
    User = oscilloscope_auth_user:get_user(User0),
    ets:member(otu, {Org#org.id, User#user.id}).
