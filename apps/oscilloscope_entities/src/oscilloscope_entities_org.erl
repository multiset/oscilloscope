-module(oscilloscope_entities_org).

-export([
    create/1,
    delete/1,
    lookup/1,
    members/1,
    is_owner/2,
    add_member/2,
    remove_member/2,
    is_member/2
]).

-include("oscilloscope_entities.hrl").

-spec create(binary()) -> {ok, tuple()}.
create(Name) when is_binary(Name) ->
    {ok,_,_,[{OwnerID}]} = oscilloscope_metadata_sql:named(insert_owner, []),
    {ok,_,_,[{ID}]} = oscilloscope_metadata_sql:named(insert_org, [Name, OwnerID]),
    {ok, #org{name=Name, id=ID, owner_id=OwnerID}}.

-spec delete(#org{}) -> ok.
delete(Org) ->
    {ok,_} = oscilloscope_metadata_sql:named(delete_org, [Org#org.id]),
    ets:delete(orgs, Org#org.name),
    ets:match_delete(org_members, {{Org#org.id, '_'}, '_'}),
    ets:match_delete(teams, {{Org#org.id, '_'}, '_'}).

-spec lookup(binary()) -> {ok, #org{}} | not_found.
lookup(Name) when is_binary(Name) ->
    case ets:lookup(orgs, Name) of
        [] ->
            not_found;
        [#org{}=Org] ->
            Org
    end.

-spec members(#org{}) -> [#user{}].
members(Org) ->
    MemberIDs = ets:match(org_members, {Org#org.id, '$1'}),
    lists:foldl(fun([MemberID], Acc) ->
        case ets:lookup(user_names, MemberID) of
            [MemberName] ->
                case oscilloscope_entities_user:lookup(MemberName) of
                    {ok, User} ->
                        [User|Acc];
                    not_found ->
                        Acc
                end;
            [] ->
                Acc
        end
    end, [], MemberIDs).

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
            oscilloscope_entities_team:is_member(User, Team, Org)
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
    ets:member(org_members, {Org#org.id, User#user.id}).
