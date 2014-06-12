-module(oscilloscope_auth).

-export([
    is_authorized/2,
    create/2,
    get_owner_id/1,
    join_team/2,
    join_org/2,
    get_teams/2
]).

-include("oscilloscope_auth.hrl").


-spec is_authorized(binary(), binary()) -> boolean().
is_authorized(Email, Pass) ->
    Match = #user{email=list_to_binary(Email), _='_'},
    case ets:match_object(user_cache, Match) of
        [#user{password=Hash}] ->
            {ok, LHash} = bcrypt:hashpw(Pass, Hash),
            Hash =:= list_to_binary(LHash);
        _ ->
            false
    end.


-spec create(binary(), binary()) -> {ok, #user{}}.
create(Email, Pass) ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, Hash} = bcrypt:hashpw(Pass, Salt),
    {ok, 1} = oscilloscope_metadata:named(
        insert_user,
        [Email, Hash]
    ),
    {ok, _, [{Id}]} = oscilloscope_metadata:named(select_user_id, [Email]),
    User = #user{
        id=Id,
        password=Hash,
        email=Email
    },
    ok = ets:insert(user_cache, User),
    {ok, User}.


-spec get_owner_id(#user{} | #org{}) -> {ok, integer()} | not_found.
get_owner_id(#user{}=User) ->
    #user{id=Id} = User,
    get_owner_id_int(Id, select_owner_id_from_user_id);
get_owner_id(#org{}=Org) ->
    #org{id=Id} = Org,
    get_owner_id_int(Id, select_owner_id_from_user_id).

get_owner_id_int(Id, Query) ->
    QueryResp = oscilloscope_metadata:named(
        Query,
        [Id]
    ),
    case QueryResp of
        {ok, _, [{OwnerID}]} ->
            {ok, OwnerID};
        _ ->
            not_found
    end.


get_teams(Org, User) ->
    ets:lookup(user_teams, {Org#org.id, User#user.id}).

-spec join_org(integer(), integer()) -> ok | {error, binary()}.
join_org(UserID, OrgID) ->
    oscilloscope_metadata:named(user_join_org, [UserID, OrgID]).


-spec join_team(integer(), integer()) -> ok | {error, binary()}.
join_team(UserID, TeamID) ->
    oscilloscope_metadata:named(user_join_team, [UserID, TeamID]).
