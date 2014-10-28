-module(osc_http_cors).
-behavior(cowboy_middleware).

-export([execute/2]).

execute(Req0, Env) ->
    Req1 = set_cors_headers(Req0),
    case cowboy_req:method(Req1) of
        {<<"OPTIONS">>, Req2} ->
            {ok, ReqFinal} = cowboy_req:reply(200, Req2),
            {halt, ReqFinal};
        {_, Req2} ->
            {ok, Req2, Env}
    end.

set_cors_headers(Req0) ->
    lists:foldl(
        fun({Header, Value}, Req) ->
            cowboy_req:set_resp_header(Header, Value, Req)
        end,
        Req0,
        application:get_env(osc_http, cors, [])
    ).
