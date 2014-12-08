-module(osc_meta_users_handler).

-export([init/3, terminate/3]).
-export([
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    from_json/2
]).

-record(st, {

}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
    ok.

rest_init(Req, _State) ->
    {ok, Req, #st{}}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

from_json(Req0, State) ->
    {ok, JSONBody, Req1} = cowboy_req:body(Req0),
    {Body} = jiffy:decode(JSONBody),
    Username = proplists:get_value(<<"username">>, Body),
    Password = proplists:get_value(<<"password">>, Body),
    {ok, UserID} = osc_meta_user:create(Username, Password),
    ok = case proplists:get_value(<<"email">>, Body) of
        undefined -> ok;
        Email -> osc_meta_user:add_email(UserID, Email)
    end,
    {{true, [<<"/users/">>, UserID]}, Req1, State}.
