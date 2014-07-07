-module(oscilloscope_auth_org).

-export([
    create/1,
    lookup/1,
    get_users/1,
    is_owner/2,
    add_member/2,
    remove_member/2,
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

-spec add_member(#org{}, #user{}) -> ok.
add_member(Org, User) ->
    {ok, _} = oscilloscope_metadata_sql:named(
        add_user_to_org,
        [Org#org.id, User#user.id]
    ),
    ets:insert(org_members, {Org#org.id, User#user.id}),
    ok.

-spec remove_member(#org{}, #user{}) -> ok.
remove_member(Org, User) ->
    {ok, _, _} = oscilloscope_metadata_sql:named(
        remove_user_from_org,
        [Org#org.id, User#user.id]
    ),
    ets:insert(org_members, {Org#org.id, User#user.id}),
    ok.

-spec is_member(#org{}, #user{}) -> boolean().
is_member(Org, User) ->
    ets:member(org_member, {Org#org.id, User#user.id}).
