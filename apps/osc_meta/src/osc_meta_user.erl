-module(osc_meta_user).

-export([
    create/2,
    delete/1,
    teams/2,
    orgs/1,
    add_email/2,
    remove_email/2,
    change_password/2
]).

-include_lib("osc/include/osc_types.hrl").

-spec hash_password(binary()) -> binary().
hash_password(Pass) ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, LHash} = bcrypt:hashpw(Pass, Salt),
    list_to_binary(LHash).

-spec create(binary(), binary()) -> {ok, user_id()}.
create(Name, Pass) ->
    Hash = hash_password(Pass),
    {ok,1,_,[{User}]} = osc_sql:named(create_user, [Name, Hash]),
    {ok, User}.

-spec add_email(user_id(), binary()) -> ok.
add_email(UserID, Email) ->
    {ok,1,_} = osc_sql:named(add_email, [UserID, Email]).

-spec remove_email(user_id(), binary()) -> ok.
remove_email(UserID, Email) ->
    {ok,_} = osc_sql:named(remove_email, [UserID, Email]).

-spec change_password(user_id(), binary()) -> ok.
change_password(UserID, NewPass) ->
    Hash = hash_password(NewPass),
    {ok,_} = osc_sql:named(change_password, [UserID, Hash]).

-spec delete(user_id()) -> ok.
delete(UserID) ->
    {ok,_} = osc_sql:named(delete_user, [UserID]).

-spec teams(org_id(), user_id()) -> [team_id()].
teams(OrgID, UserID) ->
    {ok,_} = osc_sql:named(get_user_teams, [OrgID, UserID]).

-spec orgs(user_id()) -> [org_id()].
orgs(UserID) ->
    {ok,_} = osc_sql:named(get_user_orgs, [UserID]).
