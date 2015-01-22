-module(osc_http).

-export([
    routes/0,
    load_routes/1,
    parse_json_patch/1,
    onresponse/4,
    onrequest/1,
    set_session/3,
    get_session/1,
    is_authorized/3,
    delete_session/1,
    start/0,
    stop/0
]).

routes() ->
    application:get_env(osc_http, routes).

load_routes(App) ->
    {ok, AppRoutes} = file:consult(code:priv_dir(App) ++ "/dispatch.conf"),
    {ok, LoadedRoutes} = application:get_env(osc_http, routes),
    NewRoutes = [{App, AppRoutes}|proplists:delete(App, LoadedRoutes)],
    application:set_env(osc_http, routes, NewRoutes),
    Dispatch = [{'_', lists:flatten([Routes || {_App, Routes} <- NewRoutes])}],
    cowboy:set_env(osc_http, dispatch, cowboy_router:compile(Dispatch)).


parse_json_patch(JSON) ->
    {Patch} = jiffy:decode(JSON),
    %% As documented in https://tools.ietf.org/html/rfc6902
    Operation = proplists:get_value(<<"op">>, Patch),
    RawPath = proplists:get_value(<<"path">>, Patch),
    %% Leave off the head - the leading slash is required, which leave an empty
    %% binary as the first returned element
    [_|Path] = binary:split(RawPath, <<"/">>, [global]),
    Value = proplists:get_value(<<"value">>, Patch),
    {ok, {Operation, Path, Value}}.


onresponse(Code, Headers, Body, Req0) ->
    {Start, Req1} = cowboy_req:meta(start, Req0),
    Duration = timer:now_diff(os:timestamp(), Start) div 1000,
    mstat:update_histogram([osc_http, requests, total, latency], Duration),
    mstat:increment_counter([osc_http, requests, total, count]),
    record_status_code_statistics(Code, Duration),
    {Method, Req2} = cowboy_req:method(Req1),
    record_method_statistics(Method, Duration),
    error_hook(Code, Headers, Body, Req2).

error_hook(500, Headers0, <<>>, Req0) ->
    Body = jiffy:encode({[{error, <<"An unknown error occurred.">>}]}),
    Headers1 = lists:keyreplace(
        <<"content-length">>,
        1,
        Headers0,
        {<<"content-length">>, integer_to_list(erlang:byte_size(Body))}
    ),
    Headers2 = lists:keyreplace(
        <<"content-type">>,
        1,
        Headers1,
        {<<"content-type">>, <<"application/json">>}
    ),
    {ok, Req1} = cowboy_req:reply(500, Headers2, Body, Req0),
    Req1;
error_hook(_, _, _, Req) ->
    Req.


onrequest(Req0) ->
    Req1 = cowboy_req:set_meta(start, os:timestamp(), Req0),
    session(Req1).


session(Req0) ->
    {ok, CookieName} = application:get_env(osc_http, cookie_name),
    case cowboy_req:cookie(CookieName, Req0) of
        {undefined, Req1} ->
            Req1;
        {Cookie, Req1} ->
            case osc_http_auth:inbound(Cookie) of
                undefined ->
                    Req1;
                {error, Error} ->
                    lager:warning("Got error ~p when parsing cookie", [Error]),
                    Req1;
                {ok, Data, Lifetime} ->
                    set_session(Data, Lifetime, Req1)
            end
    end.

set_session(Data, Lifetime, Req0) ->
    {ok, CookieName} = application:get_env(osc_http, cookie_name),
    {ok, CookieOpts} = application:get_env(osc_http, cookie_opts),
    {ok, CookieValue} = osc_http_auth:outbound(Data, Lifetime),
    Req1 = cowboy_req:set_meta(session, Data, Req0),
    cowboy_req:set_resp_cookie(
        CookieName,
        CookieValue,
        [{max_age, Lifetime}|CookieOpts],
        Req1
    ).

get_session(Req) ->
    cowboy_req:meta(session, Req, []).

is_authorized(Req0, SuccessFun, FailureFun) ->
    {Meta, Req1} = get_session(Req0),
    case proplists:get_value(id, Meta) of
        undefined ->
            {AuthHeader, Req2} = cowboy_req:header(<<"authorization">>, Req1),
            case AuthHeader of
                <<"Basic ", Encoded/binary>> ->
                    Decoded = base64:decode(Encoded),
                    [Username, Password] = binary:split(Decoded, <<":">>),
                    case osc_meta_user:is_authorized(Username, Password) of
                        true ->
                            {ok, UserProps} = osc_meta_user:lookup(Username),
                            UserID = proplists:get_value(id, UserProps),
                            {true, Req2, SuccessFun(UserID)};
                        false ->
                            {
                                {false, <<"Basic realm=\"oscilloscope\"">>},
                                Req2,
                                FailureFun()
                            }
                    end;
                _ ->
                    {
                        {false, <<"Basic realm=\"oscilloscope\"">>},
                        Req2,
                        FailureFun()
                    }
            end;
        UserID ->
            {true, Req1, SuccessFun(UserID)}
    end.

delete_session(Req) ->
    {ok, CookieName} = application:get_env(osc_http, cookie_name),
    {ok, CookieOpts} = application:get_env(osc_http, cookie_opts),
    cowboy_req:set_resp_cookie(
        CookieName,
        <<>>,
        [{max_age, 0}|CookieOpts],
        Req
    ).

start() ->
    application:start(osc_http).

stop() ->
    ok.

record_status_code_statistics(200, T) ->
    record_statistics([status_code, 200], T);
record_status_code_statistics(201, T) ->
    record_statistics([status_code, 201], T);
record_status_code_statistics(204, T) ->
    record_statistics([status_code, 204], T);
record_status_code_statistics(301, T) ->
    record_statistics([status_code, 301], T);
record_status_code_statistics(302, T) ->
    record_statistics([status_code, 302], T);
record_status_code_statistics(303, T) ->
    record_statistics([status_code, 303], T);
record_status_code_statistics(400, T) ->
    record_statistics([status_code, 400], T);
record_status_code_statistics(401, T) ->
    record_statistics([status_code, 401], T);
record_status_code_statistics(403, T) ->
    record_statistics([status_code, 403], T);
record_status_code_statistics(404, T) ->
    record_statistics([status_code, 404], T);
record_status_code_statistics(405, T) ->
    record_statistics([status_code, 405], T);
record_status_code_statistics(500, T) ->
    record_statistics([status_code, 500], T);
record_status_code_statistics(_, T) ->
    record_statistics([status_code, unknown], T).

record_method_statistics(<<"GET">>, T) ->
    record_statistics([method, get], T);
record_method_statistics(<<"PATCH">>, T) ->
    record_statistics([method, patch], T);
record_method_statistics(<<"POST">>, T) ->
    record_statistics([method, post], T);
record_method_statistics(<<"PUT">>, T) ->
    record_statistics([method, put], T);
record_method_statistics(<<"DELETE">>, T) ->
    record_statistics([method, delete], T);
record_method_statistics(<<"OPTIONS">>, T) ->
    record_statistics([method, options], T);
record_method_statistics(_, T) ->
    record_statistics([method, unknown], T).

record_statistics(Path0, T) ->
    Path1 = [osc_http, requests] ++ Path0,
    mstat:increment_counter(Path1 ++ [count]),
    mstat:update_histogram(Path1 ++ [latency], T).
