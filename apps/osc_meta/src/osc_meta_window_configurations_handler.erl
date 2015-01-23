-module(osc_meta_window_configurations_handler).

-export([init/3, terminate/3]).
-export([
    rest_init/2,
    allowed_methods/2,
    is_authorized/2,
    forbidden/2,
    content_types_accepted/2,
    content_types_provided/2,
    from_json/2,
    to_json/2
]).

-record(st, {
    user_id,
    org_id
}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
    ok.

rest_init(Req, _Opts) ->
    {ok, Req, #st{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

is_authorized(Req, State) ->
    osc_http:is_authorized(
        Req,
        fun(UserID) -> State#st{user_id=UserID} end,
        fun() -> State end
    ).

forbidden(Req0, State) ->
    #st{user_id=AuthUserID}=State,
    {OrgIDBin, Req1} = cowboy_req:binding(org_id, Req0),
    OrgID = list_to_integer(binary_to_list(OrgIDBin)),
    {Method, Req2} = cowboy_req:method(Req1),
    Authorized = case Method of
        <<"POST">> ->
            %% Only owners can modify window configurations
            osc_meta_org:is_owner(OrgID, AuthUserID);
        <<"GET">> ->
            %% Any member can view window configurations
            osc_meta_org:is_member(OrgID, AuthUserID)
    end,
    {not Authorized, Req2, State#st{org_id=OrgID}}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

from_json(Req0, State) ->
    {ok, JSONBody, Req1} = cowboy_req:body(Req0),
    {Body} = jiffy:decode(JSONBody),
    Priority = proplists:get_value(<<"priority">>, Body),
    {Tags} = proplists:get_value(<<"tags">>, Body),
    Windows = lists:map(
        fun({Window}) ->
            Type = osc_meta_util:parse_window_type(
                proplists:get_value(<<"type">>, Window)
            ),
            Aggregation = osc_meta_util:parse_window_aggregation(
                proplists:get_value(<<"aggregation">>, Window)
            ),
            {
                Type,
                Aggregation,
                proplists:get_value(<<"interval">>, Window),
                proplists:get_value(<<"count">>, Window)
            }
        end,
        proplists:get_value(<<"windows">>, Body, [])
    ),
    {ok, GroupID} = osc_meta_window_configuration:create(
        State#st.org_id,
        Priority,
        Tags,
        Windows
    ),
    {Path, Req2} = cowboy_req:path(Req1),
    Req3 = cowboy_req:set_resp_header(
        <<"location">>,
        [Path, <<"/">>, integer_to_list(GroupID)],
        Req2
    ),
    {true, Req3, State}.

to_json(Req, State) ->
    {ok, Configs} = osc_meta_window_configuration:list(State#st.org_id),
    %% Turn in to proper EJSON
    EJSON = lists:map(
        fun(Props0) ->
            {value, {windows, Windows0}, Props1} = lists:keytake(
                windows,
                1,
                Props0
            ),
            Windows1 = [{W} || W <- Windows0],
            {value, {tags, Tags}, Props2} = lists:keytake(tags, 1, Props1),
            {[{windows, Windows1}, {tags, {Tags}}] ++ Props2}
        end,
        Configs
    ),
    {jiffy:encode(EJSON), Req, State}.
