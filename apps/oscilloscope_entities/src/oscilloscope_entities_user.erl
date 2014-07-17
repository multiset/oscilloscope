-module(oscilloscope_entities_user).

-export([
    create/3,
    delete/1,
    get_teams/2
]).

-include("oscilloscope_entities.hrl").


-spec create(binary(), binary(), binary()) -> {ok, #user{}}.
create(Name, Email, Pass) ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, LHash} = bcrypt:hashpw(Pass, Salt),
    BHash = list_to_binary(LHash),
    {ok,_,_,[{OwnerID}]} = oscilloscope_metadata_sql:named(insert_owner, []),
    {ok,_,_,[{UserID}]} = oscilloscope_metadata_sql:named(
        insert_user,
        [Name, Email, BHash, OwnerID]
    ),
    User = #user{
        id=UserID,
        name=Name,
        owner_id=OwnerID,
        password=BHash,
        email=Email
    },
    true = ets:lookup(user_names, UserID),
    true = ets:insert(users, User),
    {ok, User}.

-spec delete(#user{}) -> ok.
delete(User) ->
    oscilloscope_metadata_sql:named(delete_user, []),
    true = ets:delete(User#user.name, users),
    ok.

-spec get_teams(#org{}, #user{}) -> [#team{}].
get_teams(Org, User) ->
    TeamIDs = ets:lookup(user_teams, {Org#org.id, User#user.id}),
    lists:foldl(fun([TeamID], Acc) ->
        case ets:lookup(team_names, {Org#org.id, TeamID}) of
            [TeamName] ->
                case oscilloscope_entities_team:lookup(TeamName) of
                    {ok, Team} ->
                        [Team|Acc];
                    not_found ->
                        Acc
                end;
            [] ->
                Acc
        end
    end, [], TeamIDs).
