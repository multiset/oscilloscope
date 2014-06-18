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
    {ok,_,_,[{OwnerID}]} = oscilloscope_metadata_sql:named(insert_owner, []),
    {ok,_,_,[{ID}]} = oscilloscope_metadata_sql:named(insert_org, [Name, OwnerID]),
    {ok, #org{name=Name, id=ID, owner_id=OwnerID}}.

-spec lookup(binary()) -> {ok, #org{}} | not_found.
lookup(Name) when is_binary(Name) ->
    case ets:lookup(orgs, Name) of
        [] ->
            not_found;
        [#org{}=Org] ->
            Org
    end.

get_users(OrgID) ->
    oscilloscope_metadata_sql:named(get_users, [OrgID]).


-spec is_owner(#user{}, #org{}) -> boolean().
is_owner(User, Org) ->
    case ets:lookup(teams, {Org#org.id, <<"owners">>}) of
        [] ->
            lager:error(
                "owners team not in ets table for org: id: ~p, name: ~p",
                [Org#org.id, Org#org.name]
            ),
            false;
        [#team{}=Team] ->
            oscilloscope_auth_team:is_member(User, Team, Org)
    end.

-spec add_members(integer() | #org{} | binary(), integer()) -> ok.
add_members(Org0, Users0) ->
    Org = lookup(Org0),
    Users = [oscilloscope_auth_user:get_user(User) || User <- Users0],
    {ok, _, _} = oscilloscope_metadata_sql:named(
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
        {ok, _, _} = oscilloscope_metadata_sql:named(
            remove_user_from_teams,
            [User#user.id|TeamIDs]
        )
    end, ok, Users),
    {ok, _, _} = oscilloscope_metadata_sql:named(
        remove_users_from_org,
        [Org#org.id|[User#user.id || User <- Users]]
    ),

    ok.

-spec is_member(binary() | #org{} | integer(), binary() | #user{} | integer()) -> boolean().
is_member(Org0, User0) ->
    Org = lookup(Org0),
    User = oscilloscope_auth_user:get_user(User0),
    ets:member(otu, {Org#org.id, User#user.id}).
