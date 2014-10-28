-module(osc_http).

-export([
    routes/0,
    load_routes/1,
    parse_json_patch/1,
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

start() ->
    application:start(osc_http).

stop() ->
    ok.
