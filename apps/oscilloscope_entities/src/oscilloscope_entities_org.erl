-module(oscilloscope_entities_org).

-export([
    create/1,
    delete/1,
    lookup/1,
    teams/1,
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
    Org = #org{name=Name, id=ID, owner_id=OwnerID, members=[], teams=[]},
    true = ets:insert(orgs, Org),
    true = ets:insert(org_names, {ID, Name}),
    {ok, Org}.

-spec delete(#org{}) -> ok.
delete(Org) ->
    #org{id=OrgID, name=OrgName} = Org,
    true = ets:delete(orgs, OrgName),
    true = ets:delete(org_names, OrgID),
    lists:map(fun(User) -> 
        NewUser = User#user{orgs=dict:erase(OrgID, User#user.orgs)},
        ets:insert(users, NewUser)
    end, members(Org)),
    [oscilloscope_entities_team:delete(Team) || Team <- teams(Org)],
    {ok, _} = oscilloscope_metadata_sql:named(delete_org, [OrgID]),
    {ok, _} = oscilloscope_metadata_sql:named(delete_org_members, [OrgID]),
    ok.

-spec teams(#org{}) -> [#team{}].
teams(Org) ->
    lists:foldl(fun(TeamID, Acc) ->
        case oscilloscope_entities_team:lookup(Org, TeamID) of
            {ok, Team} ->
                [Team|Acc];
            not_found ->
                lager:error(
                    "Team not found in thing: ~p",
                    [{Org#org.id, TeamID}]
                ),
                Acc
        end
    end, [], Org#org.teams).

-spec lookup(binary() | integer()) -> {ok, #org{}} | not_found.
lookup(Name) when is_binary(Name) ->
    case ets:lookup(orgs, Name) of
        [] ->
            not_found;
        [#org{}=Org] ->
            {ok, Org}
    end;
lookup(OrgID) when is_integer(OrgID) ->
    case ets:lookup(org_names, OrgID) of
        [] ->
            not_found;
        [{_,OrgName}] ->
            lookup(OrgName)
    end.

-spec members(#org{}) -> [#user{}].
members(Org) ->
    lists:foldl(fun(UserID, Acc) ->
        case oscilloscope_entities_user:lookup(UserID) of
            {ok, User} ->
                [User|Acc];
            not_found ->
                Acc
        end
    end, [], Org#org.members).

-spec is_owner(#user{}, #org{}) -> boolean().
is_owner(User, Org) ->
    case oscilloscope_entities_team:lookup(Org, <<"owners">>) of
        not_found ->
            lager:error(
                "owners team not in ets table for org: id: ~p, name: ~p",
                [Org#org.id, Org#org.name]
            ),
            false;
        {ok, Team} ->
            oscilloscope_entities_team:is_member(Team, User)
    end.

-spec add_member(#org{}, #user{}) -> {ok, #org{}, #user{}}.
add_member(Org, User) ->
    #org{members=Members, id=OrgID} = Org,
    #user{orgs=Orgs, id=UserID} = User,
    {ok, _} = oscilloscope_metadata_sql:named(
        add_user_to_org,
        [Org#org.id, User#user.id]
    ),
    NewOrg = Org#org{members=[UserID|Members]},
    NewUser = User#user{orgs=dict:store(OrgID, [], Orgs)},
    ets:insert(orgs, NewOrg),
    ets:insert(users, NewUser),
    {ok, NewOrg, NewUser}.

-spec remove_member(#org{}, #user{}) -> {ok, #org{}, #user{}}.
remove_member(Org, User) ->
    TeamIDs = dict:find(Org#org.id, User#user.orgs),
    lists:map(fun(TeamID) ->
        case oscilloscope_entities_team:lookup(TeamID) of
            not_found ->
                lager:error(
                    "Nonexistent team in #user{id=~p} record found while "
                    "removing member from org",
                    [User#user.id]
                );
            {ok, Team} ->
                oscilloscope_entities_team:remove_member(Team, User)
        end
    end, TeamIDs),
    NewOrg = Org#org{members=lists:delete(User#user.id, Org#org.members)},
    NewUser = User#user{orgs=dict:erase(Org#org.id, User#user.orgs)},
    ets:insert(orgs, NewOrg),
    ets:insert(users, NewUser),
    {ok, _} = oscilloscope_metadata_sql:named(
        remove_user_from_org,
        [Org#org.id, User#user.id]
    ),
    lists:foldl(fun(Team, UserAcc) ->
        {ok, _, UserAcc0} = oscilloscope_entities_team:remove_member(Team, UserAcc),
        UserAcc0
    end, User, teams(Org)),
    {ok, NewOrg, NewUser}.

-spec is_member(#org{}, #user{}) -> boolean().
is_member(Org, User) ->
    dict:is_key(Org#org.id, User#user.orgs).
