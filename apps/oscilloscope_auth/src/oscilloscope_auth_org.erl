-module(oscilloscope_auth_org).

-export([create/1, get_id/1, get_users/1]).

-include("oscilloscope_auth.hrl").


-spec create(binary()) -> {ok, tuple()}.
create(Name) ->
    {ok, 1} = oscilloscope_metadata:named(create_org, [Name]),
    {ok, _, [{Id}]} = oscilloscope_metadata:named(select_org_id, [Name]),
    {ok, #org{name=Name, id=Id}}.

-spec get_id(binary()) -> {ok, integer()} | not_found.
get_id(Name) ->
    oscilloscope_metadata:named(get_org_id, [Name]).

get_users(OrgID) ->
    oscilloscope_metadata:named(get_users, [OrgID]).
