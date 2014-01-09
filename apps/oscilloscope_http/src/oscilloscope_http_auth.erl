-module(oscilloscope_http_auth).

-export([get_authorized_user/1, is_authorized/2]).

-include("oscilloscope_http.hrl").

get_authorized_user(Req) ->
    case wrq:get_req_header("authorization", Req) of
        "Basic " ++ Base64 ->
            Str = base64:mime_decode_to_string(Base64),
            case string:tokens(Str, ":") of
                [Name0, Pass0] ->
                    Name = list_to_binary(Name0),
                    Pass = list_to_binary(Pass0),
                    case ets:match_object(user_cache, #user{name=Name, _='_'}) of
                        [#user{password=Pass}=User] ->
                            User;
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
