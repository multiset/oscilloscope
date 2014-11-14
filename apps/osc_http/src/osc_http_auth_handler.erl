-module(osc_http_auth_handler).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req0, State) ->
    {Action, Req1} = cowboy_req:binding(action, Req0),
    {ok, Req2} = dispatch(Action, Req1),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

dispatch(<<"login">>, Req0) ->
    case cowboy_req:method(Req0) of
        {<<"POST">>, Req1} ->
            {ok, Data, Req2} = cowboy_req:body(Req1),
            {Props} = jiffy:decode(Data),
            Username = proplists:get_value(<<"username">>, Props, <<>>),
            Password = proplists:get_value(<<"password">>, Props, <<>>),
            Authorized = osc_meta_user:is_authorized(
                Username,
                Password
            ),
            case Authorized of
                true ->
                    {ok, DefaultLifetime} = application:get_env(
                        osc_http,
                        default_cookie_lifetime
                    ),
                    Lifetime = proplists:get_value(
                        <<"lifetime">>,
                        Props,
                        DefaultLifetime
                    ),
                    Req3 = osc_http:set_session(
                        [{username, Username}],
                        Lifetime,
                        Req2
                    ),
                    cowboy_req:reply(204, [], <<>>, Req3);
                false ->
                    cowboy_req:reply(401, [], <<>>, Req2)
            end;
        {_, Req1} ->
            cowboy_req:reply(405, [{<<"allow">>, [<<"POST">>]}], <<>>, Req1)
    end;
dispatch(<<"logout">>, Req0) ->
    Req1 = osc_http:delete_session(Req0),
    {ok, Req1};
dispatch(<<"whoami">>, Req0) ->
    {Meta, Req1} = osc_http:get_session(Req0),
    Body = case Meta of
        undefined ->
            [{error, unknown}];
        _ ->
            [{username, proplists:get_value(username, Meta)}]
    end,
    cowboy_req:reply(
        200,
        [{<<"content-type">>, <<"application/json">>}],
        jiffy:encode({Body}),
        Req1
    ).
