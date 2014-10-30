-module(osc_http_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cowboy:start_http(
        osc_http,
        100,
        [{port, 9000}],
        [
            {env, [{dispatch, cowboy_router:compile([])}]},
            {middlewares, [cowboy_router, osc_http_cors, cowboy_handler]},
            {onresponse, fun osc_http:error_hook/4}
        ]
    ),
    osc_http:load_routes(osc_http),
    osc_http_sup:start_link().

stop(_State) ->
    ok.
