-module(osc_meta_user).

-export([
    lookup/1,
    create/2,
    delete/1,
    teams/1,
    teams/2,
    orgs/1,
    add_email/2,
    remove_email/2,
    change_password/2,
    is_authorized/2
]).

-include_lib("osc/include/osc_types.hrl").

-spec lookup(binary() | user_id()) -> {ok, [{any(), any()}]} | not_found.
lookup(NameOrID) ->
    [{ok, _, UserLookup}, {ok, _, Emails}] = if is_integer(NameOrID) ->
        osc_sql:batch([
            {get_user_by_id, [NameOrID]},
            {get_emails_by_id, [NameOrID]}
        ]);
    true ->
        osc_sql:batch([
            {get_user_by_name, [NameOrID]},
            {get_emails_by_name, [NameOrID]}
        ])
    end,
    case UserLookup of
        [] ->
            not_found;
        [{UserID, OwnerID, Name, Pass, _Active}] ->
            {ok, [
                {id, UserID},
                {owner_id, OwnerID},
                {name, Name},
                {password, Pass},
                {emails, [E || {E} <- Emails]}
            ]}
    end.

-spec create(binary(), binary()) -> {ok, user_id()}.
create(Name, Pass) ->
    Hash = hash_password(Pass),
    {ok,1,_,[{UserID}]} = osc_sql:named(create_user, [Name, Hash]),
    {ok, UserID}.

-spec delete(user_id()) -> ok.
delete(UserID) ->
    [{ok,_},{ok,1},{ok,_},{ok,_}] = osc_sql:batch([
        {delete_emails, [UserID]},
        {delete_user, [UserID]},
        {remove_user_from_all_orgs, [UserID]},
        {remove_user_from_all_teams, [UserID]}
    ]),
    ok.

-spec add_email(user_id(), binary()) -> ok.
add_email(UserID, Email) ->
    {ok,1} = osc_sql:named(add_email, [UserID, Email]),
    ok.

-spec remove_email(user_id(), binary()) -> ok.
remove_email(UserID, Email) ->
    {ok,1} = osc_sql:named(remove_email, [UserID, Email]),
    ok.

-spec change_password(user_id(), binary()) -> ok.
change_password(UserID, NewPass) ->
    Hash = hash_password(NewPass),
    {ok,_} = osc_sql:named(change_password, [UserID, Hash]),
    ok.

-spec teams(user_id()) -> [{team_id(), binary(), non_neg_integer()}].
teams(UserID) ->
    {ok, _, Rows} = osc_sql:named(get_user_teams, [UserID]),
    Rows.

-spec teams(org_id(), user_id()) -> [{team_id(), binary(), non_neg_integer()}].
teams(OrgID, UserID) ->
    {ok, _, Rows} = osc_sql:named(get_user_org_teams, [OrgID, UserID]),
    Rows.

-spec orgs(user_id()) -> [{org_id(), binary()}].
orgs(UserID) ->
    {ok, _, Rows} = osc_sql:named(get_user_orgs, [UserID]),
    Rows.

-spec hash_password(binary()) -> binary().
hash_password(Pass) ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, LHash} = bcrypt:hashpw(Pass, Salt),
    list_to_binary(LHash).

-spec is_authorized(binary(), binary()) -> boolean().
is_authorized(Name, Pass) ->
    case lookup(Name) of
        {ok, User} ->
            Hash = proplists:get_value(password, User),
            {ok, LHash} = bcrypt:hashpw(Pass, Hash),
            Hash =:= list_to_binary(LHash);
        _ ->
            false
    end.
