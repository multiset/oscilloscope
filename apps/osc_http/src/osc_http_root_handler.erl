-module(osc_http_root_handler).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req0, State) ->
    {ok, Req1} = cowboy_req:reply(
        200,
        [{<<"content-type">>, <<"text/plain">>}],
        <<"Hello world!">>,
        Req0
    ),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.
