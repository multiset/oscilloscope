-module(osc_http).

-export([
    routes/0,
    load_routes/1,
    parse_json_patch/1,
    error_hook/4,
    session/1,
    set_session/3,
    get_session/1,
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

session(Req0) ->
    {ok, CookieName} = application:get_env(osc_http, cookie_name),
    case cowboy_req:cookie(CookieName, Req0) of
        undefined ->
            Req0;
        {Cookie, Req1} ->
            case osc_http_auth:inbound(Cookie) of
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
    cowboy_req:meta(session, Req).

start() ->
    application:start(osc_http).

stop() ->
    ok.
