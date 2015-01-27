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

-spec lookup(NameOrID) -> {ok, Props} | not_found when
    NameOrID :: binary() | user_id(),
    Props :: proplists:proplist().

lookup(Name) when is_binary(Name) ->
    SQL = "SELECT id, password FROM users WHERE name = $1;",
    {ok, _, User} = mpgsql:equery(SQL, [Name]),
    case User of
        [] ->
            not_found;
        [{UserID, Password}] ->
            {ok, [
                {id, UserID},
                {name, Name},
                {password, Password},
                {emails, emails(UserID)}
            ]}
    end;
lookup(UserID) ->
    SQL = "SELECT name, password FROM users WHERE id = $1;",
    {ok, _, User} = mpgsql:equery(SQL, [UserID]),
    case User of
        [] ->
            not_found;
        [{Name, Password}] ->
            {ok, [
                {id, UserID},
                {name, Name},
                {password, Password},
                {emails, emails(UserID)}
            ]}
    end.


-spec emails(UserID) -> Emails when
    UserID :: user_id(),
    Emails :: [binary()].

emails(UserID) ->
    EmailSQL = "SELECT email FROM emails WHERE user_id = $1;",
    {ok, _, Emails} = mpgsql:equery(EmailSQL, [UserID]),
    [E || {E} <- Emails].


-spec create(Username, Password) -> {ok, UserID} | {error, Error} when
    Username :: binary(),
    Password :: binary(),
    UserID :: user_id(),
    Error :: exists.

create(Username, Password) ->
    Hash = hash_password(Password),
    SQL = "INSERT INTO users (name, password) VALUES ($1, $2) RETURNING id;",
    case mpgsql:equery(SQL, [Username, Hash]) of
        {error, unique_violation} ->
            {error, exists};
        {ok, 1 , _, [{UserID}]} ->
            mstat:increment_counter([osc_meta, creations, user]),
            {ok, UserID}
    end.


-spec delete(user_id()) -> ok.

delete(UserID) ->
    ok = mpgsql:tx_begin(),
    RemoveFromTeamsSQL = "DELETE FROM team_members WHERE user_id = $1;",
    RemoveFromOrgsSQL = "DELETE FROM org_members WHERE user_id = $1;",
    DeleteEmailSQL = "DELETE FROM emails WHERE user_id = $1;",
    DeleteUserSQL = "DELETE FROM users WHERE id = $1;",
    {ok, _} = mpgsql:equery(RemoveFromTeamsSQL, [UserID]),
    {ok, _} = mpgsql:equery(RemoveFromOrgsSQL, [UserID]),
    {ok, _} = mpgsql:equery(DeleteEmailSQL, [UserID]),
    {ok, _} = mpgsql:equery(DeleteUserSQL, [UserID]),
    ok = mpgsql:tx_commit(),
    ok.


-spec add_email(user_id(), binary()) -> ok.

add_email(UserID, Email) ->
    SQL = "INSERT INTO emails (user_id, email) VALUES ($1, $2);",
    {ok, 1} = mpgsql:equery(SQL, [UserID, Email]),
    ok.


-spec remove_email(user_id(), binary()) -> ok.

remove_email(UserID, Email) ->
    SQL = "DELETE FROM emails WHERE user_id = $1 AND email = $2;",
    {ok, 1} = mpgsql:equery(SQL, [UserID, Email]),
    ok.


-spec change_password(user_id(), binary()) -> ok.

change_password(UserID, NewPass) ->
    Hash = hash_password(NewPass),
    SQL = "UPDATE users "
          "SET (password, updated)=($2, (now() at time one 'utc')) "
          "WHERE id = $1;",
    {ok, _} = mpgsql:equery(SQL, [UserID, Hash]),
    ok.


-spec teams(UserID) -> Teams when
    UserID :: user_id(),
    Teams :: [{TeamID, TeamName, TeamMembers}],
    TeamID :: team_id(),
    TeamName :: binary(),
    TeamMembers :: non_neg_integer().

teams(UserID) ->
    SQL = "SELECT teams.id, teams.name, "
          "(SELECT COUNT(*) FROM team_members WHERE team_id = teams.id) "
          "FROM teams JOIN team_members ON teams.id = team_members.team_id "
          "WHERE team_members.user_id = $1;",
    {ok, _, Rows} = mpgsql:equery(SQL, [UserID]),
    Rows.


-spec teams(OrgID, UserID) -> Teams when
    OrgID :: org_id(),
    UserID :: user_id(),
    Teams :: [{TeamID, TeamName, TeamMembers}],
    TeamID :: team_id(),
    TeamName :: binary(),
    TeamMembers :: non_neg_integer().

teams(OrgID, UserID) ->
    SQL = "SELECT teams.id, teams.name, "
          "(SELECT COUNT(*) FROM team_members WHERE team_id = teams.id) "
          "FROM teams JOIN team_members ON teams.id = team_members.team_id "
          "WHERE teams.org_id = $1 AND team_members.user_id = $2;",
    {ok, _, Rows} = mpgsql:equery(SQL, [OrgID, UserID]),
    Rows.


-spec orgs(UserID) -> Orgs when
    UserID :: user_id(),
    Orgs :: [{OrgID, OrgName}],
    OrgID :: org_id(),
    OrgName :: binary().

orgs(UserID) ->
    SQL = "SELECT orgs.id, orgs.name FROM orgs "
          "JOIN org_members ON orgs.id = org_members.org_id "
          "WHERE org_members.user_id = $1;",
    {ok, _, Rows} = mpgsql:equery(SQL, [UserID]),
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
