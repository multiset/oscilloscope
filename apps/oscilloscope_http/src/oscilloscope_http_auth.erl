-module(oscilloscope_http_auth).

-export([get_authorized_user/1, is_authorized/2]).

-include("oscilloscope_http.hrl").

get_authorized_user(Req) ->
    case wrq:get_req_header("authorization", Req) of
        "Basic " ++ Base64 ->
            case string:tokens(base64:mime_decode_to_string(Base64), ":") of
                [Name, Pass] ->
                    Match = #user{name=list_to_binary(Name), _='_'},
                    case ets:match_object(user_cache, Match) of
                        [#user{password=Hash}=User] ->
                            {ok, LHash} = bcrypt:hashpw(Pass, Hash),
                            case Hash =:= list_to_binary(LHash) of
                                true ->
                                    User;
                                false ->
                                    "Basic realm=webmachine"
                            end;
                        _ ->
                            "Basic realm=webmachine"
                    end;
                _ ->
                    "Basic realm=webmachine"
            end;
        _ ->
            "Basic realm=webmachine"
    end.

is_authorized(Req, State) ->
    case get_authorized_user(Req) of
        #user{}=User ->
            {User, Req, State};
        Other ->
            {Other, Req, State}
    end.
