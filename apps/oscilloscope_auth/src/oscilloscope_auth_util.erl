-module(oscilloscope_auth_util).

-export([get_authorized_user/1, is_authorized/2, parse_body/1]).

-include("oscilloscope_auth.hrl").

get_authorized_user(Req) ->
    case wrq:get_req_header("authorization", Req) of
        "Basic " ++ Base64 ->
            case string:tokens(base64:mime_decode_to_string(Base64), ":") of
                [Name, Pass] ->
                    Match = #user{email=list_to_binary(Name), _='_'},
                    case ets:match_object(user_cache, Match) of
                        [#user{password=Hash}=User] ->
                            {ok, LHash} = bcrypt:hashpw(Pass, Hash),
                            case Hash =:= list_to_binary(LHash) of
                                true ->
                                    User;
                                false ->
                                    "Basic realm=oscilloscope"
                            end;
                        _ ->
                            "Basic realm=oscilloscope"
                    end;
                _ ->
                    "Basic realm=oscilloscope"
            end;
        _ ->
            "Basic realm=oscilloscope"
    end.

is_authorized(Req, State) ->
    case get_authorized_user(Req) of
        #user{}=User ->
            {User, Req, State};
        Other ->
            {Other, Req, State}
    end.

-spec parse_body(binary()) -> [{binary(), binary()}] | false.
parse_body(Body) ->
    parse_body(binary:split(Body, <<"&">>, [global]), []).

parse_body([], Acc) ->
    Acc;
parse_body([Pair|Pairs], Acc) ->
    case binary:split(Pair, <<"=">>) of
        Split when length(Split) =:= 2 ->
            parse_body(Pairs, [list_to_tuple(Split)|Acc]);
        _ ->
            false
    end.
