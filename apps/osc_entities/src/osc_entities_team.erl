-module(osc_entities_team).

-export([
    create/2,
    delete/2,
    lookup/2,
    members/1,
    add_member/2,
    remove_member/2,
    is_member/2
]).

-include("osc_entities.hrl").


-spec create(#org{}, binary()) -> {ok, #org{}, #team{}}.
create(Org, Name) ->
    #org{id=OrgID} = Org,
    {ok, 1, _, [{ID}]} = osc_metadata_sql:named(insert_team, [Name, OrgID]),
    Team = #team{key={OrgID, Name}, id=ID, members=[]},
    NewOrg = Org#org{teams=[ID|Org#org.teams]},
    true = ets:insert(orgs, NewOrg),
    true = ets:insert(teams, Team),
    true = ets:insert(team_names, {ID, Name}),
    {ok, NewOrg, Team}.

-spec delete(#org{}, #team{}) -> {ok, #org{}}.
delete(#org{id=OrgID}=Org, #team{key={OrgID,_}}=Team) ->
    #team{id=TeamID, key={_, TeamName}} = Team,
    true = ets:delete(teams, {OrgID, TeamName}),
    true = ets:delete(team_names, {OrgID, TeamID}),
    lists:foldl(fun(User, TeamAcc) ->
        {ok, TeamAcc1, _User} = remove_member(TeamAcc, User),
        TeamAcc1
    end, Team, members(Team)),
    NewOrg = Org#org{teams=lists:delete(TeamID, Org#org.teams)},
    true = ets:insert(orgs, NewOrg),
    {ok, _} = osc_metadata_sql:named(delete_team, [TeamID]),
    {ok, _} = osc_metadata_sql:named(delete_team_members, [TeamID]),
    {ok, NewOrg}.

-spec lookup(#org{}, binary()) -> {ok, integer()} | not_found.
lookup(Org, TeamName) ->
    case ets:lookup(teams, {Org#org.id, TeamName}) of
        [] ->
            not_found;
        [#team{}=Team] ->
            {ok, Team}
    end.

-spec members(#team{}) -> [#user{}].
members(Team) ->
    lists:foldl(fun(UserID, Acc) ->
        case osc_entities_user:lookup(UserID) of
            {ok, User} ->
                [User|Acc];
            not_found ->
                Acc
        end
    end, [], Team#team.members).


-spec add_member(#team{}, #user{}) -> {ok, #team{}, #user{}}.
add_member(Team, User) ->
    #team{id=TeamID, key={OrgID,_}, members=Members} = Team,
    #user{id=UserID, orgs=Orgs} = User,
    NewTeam = Team#team{members=[UserID|Members]},
    NewUser = User#user{orgs=dict:append(OrgID, TeamID, Orgs)},
    ets:insert(users, NewUser),
    ets:insert(teams, NewTeam),
    {ok, _} = osc_metadata_sql:named(
        add_user_to_team,
        [OrgID, TeamID, UserID]
    ),
    {ok, NewTeam, NewUser}.

-spec remove_member(#team{}, #user{}) -> {ok, #team{}, #user{}}.
remove_member(Team, User) ->
    #team{id=TeamID, key={OrgID,_}, members=Members} = Team,
    #user{id=UserID, orgs=Orgs} = User,
    NewOrgs = dict:update(
        OrgID,
        fun(Teams) -> lists:delete(TeamID, Teams) end,
        Orgs
    ),
    NewUser = User#user{orgs=NewOrgs},
    NewTeam = Team#team{members=lists:delete(UserID, Members)},
    ets:insert(users, NewUser),
    ets:insert(teams, NewTeam),
    {ok, _} = osc_metadata_sql:named(
        remove_user_from_team,
        [TeamID, UserID]
    ),
    {ok, NewTeam, NewUser}.

-spec is_member(#team{}, #user{}) -> boolean().
is_member(Team, User) ->
    lists:member(User#user.id, Team#team.members).
