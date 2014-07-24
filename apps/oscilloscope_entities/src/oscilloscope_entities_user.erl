-module(oscilloscope_entities_user).

-export([
    create/2,
    delete/1,
    lookup/1,
    teams/2,
    orgs/1,
    add_email/2,
    remove_email/2,
    change_password/2
]).

-include("oscilloscope_entities.hrl").

-spec hash_password(binary()) -> binary().
hash_password(Pass) ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, LHash} = bcrypt:hashpw(Pass, Salt),
    list_to_binary(LHash).


-spec create(binary(), binary()) -> {ok, #user{}}.
create(Name, Pass) ->
    Hash = hash_password(Pass),
    {ok,_,_,[{OwnerID}]} = oscilloscope_metadata_sql:named(insert_owner, []),
    {ok,_,_,[{UserID}]} = oscilloscope_metadata_sql:named(
        insert_user,
        [Name, Hash, OwnerID]
    ),
    User = #user{
        id=UserID,
        name=Name,
        owner_id=OwnerID,
        password=Hash,
        emails=[],
        orgs=dict:new()
    },
    true = ets:insert(users, User),
    true = ets:insert(user_names, {UserID, Name}),
    {ok, User}.

-spec add_email(#user{}, binary()) -> {ok, #user{}}.
add_email(User, Email) ->
    case lists:member(Email, User#user.emails) of
        true ->
            {ok, User};
        false ->
            #user{emails=OldEmails, id=UserID} = User,
            oscilloscope_metadata_sql:named(insert_email, [UserID, Email]),
            NewUser = User#user{emails=[Email|OldEmails]},
            true = ets:insert(users, NewUser),
            {ok, NewUser}
    end.

-spec remove_email(#user{}, binary()) -> {ok, #user{}}.
remove_email(User, EmailToRemove) ->
    #user{emails=OldEmails} = User,
    Emails = lists:filter(fun(Email) -> EmailToRemove =/= Email end, OldEmails),
    NewUser = User#user{emails=Emails},
    true = ets:insert(users, NewUser),
    {ok, NewUser}.

-spec change_password(#user{}, binary()) -> {ok, #user{}}.
change_password(User, NewPassword) ->
    Hash = hash_password(NewPassword),
    {ok, _} = oscilloscope_metadata_sql:named(
        update_password,
        [User#user.id, Hash]
    ),
    NewUser = User#user{password=Hash},
    true = ets:insert(users, NewUser),
    {ok, NewUser}.

-spec lookup(binary() | integer()) -> {ok, #user{}} | not_found.
lookup(Name) when is_binary(Name) ->
    case ets:lookup(users, Name) of
        [User] ->
            {ok, User};
        [] ->
            not_found
    end;
lookup(UserID) when is_integer(UserID) ->
    case ets:lookup(user_names, UserID) of
        [] ->
            not_found;
        [{_,UserName}] ->
            lookup(UserName)
    end.

-spec delete(#user{}) -> ok.
delete(User) ->
    dict:fold(fun(OrgID, _TeamIDs, UserAcc) ->
        {ok, Org} = oscilloscope_entities_org:lookup(OrgID),
        {ok, NewUser} = oscilloscope_entities_org:remove_member(Org, UserAcc),
        NewUser
    end, ok, User#user.orgs),
    true = ets:delete(users, User#user.name),
    true = ets:delete(user_names, User#user.id),
    oscilloscope_metadata_sql:named(delete_user, []),
    ok.

-spec teams(#org{}, #user{}) -> [#team{}].
teams(Org, User) ->
    case dict:find(Org#org.id, User#user.orgs) of
        error ->
            [];
        {ok, TeamIDs} ->
            lists:foldl(fun(TeamID, Acc) ->
                case oscilloscope_entities_team:lookup(TeamID) of
                    not_found ->
                        Acc;
                    {ok, Team} ->
                        [Team|Acc]
                end
            end, [], TeamIDs)
    end.

-spec orgs(#user{}) -> [#org{}].
orgs(User) ->
    OrgIDs = dict:fetch_keys(User#user.orgs),
    lists:foldl(fun(OrgID, Acc) ->
        case oscilloscope_entities_org:lookup(OrgID) of
            not_found ->
                Acc;
            {ok, Org} ->
                [Org|Acc]
        end
    end, [], OrgIDs).
