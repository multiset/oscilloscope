-module(osc_meta_orgs_handler).

-export([init/3, terminate/3]).
-export([
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    from_json/2,
    to_json/2
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
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

from_json(Req0, State) ->
    %% TODO
    {{true, [<<"/orgs/">>, 1]}, Req0, State}.

to_json(Req0, State) ->
    {Meta, Req1} = osc_http:get_session(Req0),
    UserID = proplists:get_value(id, Meta),
    Orgs = lists:map(
        fun({ID, Name}) ->
            Owner = osc_meta_org:is_owner(ID, UserID),
            {[{id, ID}, {name, Name}, {owner, Owner}]}
        end,
        osc_meta_user:orgs(UserID)
    ),
    {jiffy:encode(Orgs), Req1, State}.
