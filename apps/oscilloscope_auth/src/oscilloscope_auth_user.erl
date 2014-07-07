-module(oscilloscope_auth_user).

-export([
    is_authorized/2,
    create/3,
    get_owner_id/1,
    join_team/2,
    join_org/2,
    get_teams/2
]).

-include("oscilloscope_auth.hrl").


-spec is_authorized(binary(), binary()) -> boolean().
is_authorized(Name, Pass) ->
    Match = #user{name=list_to_binary(Name), _='_'},
    case ets:match_object(user_cache, Match) of
        [#user{password=Hash}] ->
            {ok, LHash} = bcrypt:hashpw(Pass, Hash),
            Hash =:= list_to_binary(LHash);
        _ ->
            false
    end.


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
    true = ets:insert(user_cache, User),
    {ok, User}.


-spec get_owner_id(#user{} | #org{}) -> {ok, integer()} | not_found.
get_owner_id(#user{}=User) ->
    #user{id=Id} = User,
    get_owner_id_int(Id, select_owner_id_from_user_id);
get_owner_id(#org{}=Org) ->
    #org{id=Id} = Org,
    get_owner_id_int(Id, select_owner_id_from_user_id).

get_owner_id_int(Id, Query) ->
    QueryResp = oscilloscope_metadata_sql:named(
        Query,
        [Id]
    ),
    case QueryResp of
        {ok, _, [{OwnerID}]} ->
            {ok, OwnerID};
        _ ->
            not_found
    end.


-spec get_teams(#org{}, #user{}) -> [#team{}].
get_teams(Org, User) ->
    ets:lookup(user_teams, {Org#org.id, User#user.id}).

-spec join_org(integer(), integer()) -> ok | {error, binary()}.
join_org(UserID, OrgID) ->
    oscilloscope_metadata_sql:named(user_join_org, [UserID, OrgID]).


-spec join_team(integer(), integer()) -> ok | {error, binary()}.
join_team(UserID, TeamID) ->
    oscilloscope_metadata_sql:named(user_join_team, [UserID, TeamID]).
