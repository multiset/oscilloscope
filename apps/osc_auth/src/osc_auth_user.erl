-module(osc_auth_user).

-export([is_authorized/2, can_delete/2]).

-spec is_authorized(binary(), binary()) -> boolean().
is_authorized(Name, Pass) ->
    case osc_meta_user:lookup(Name) of
        {ok, User} ->
            Hash = proplists:get_value(password, User),
            {ok, LHash} = bcrypt:hashpw(Pass, Hash),
            Hash =:= list_to_binary(LHash);
        _ ->
            false
    end.

-spec can_delete(binary(), binary()) -> boolean().
can_delete(AuthedUser, User) ->
    AuthedUser =:= User.
