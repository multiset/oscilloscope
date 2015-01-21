-module(osc_meta_org_handler).

-export([init/3, terminate/3]).
-export([
    rest_init/2,
    allowed_methods/2,
    is_authorized/2,
    forbidden/2,
    resource_exists/2,
    content_types_accepted/2,
    content_types_provided/2,
    from_json_patch/2,
    to_json/2
]).

-record(st, {
    user_id,
    org_props,
    op,
    path,
    value
}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
    ok.

rest_init(Req0, _) ->
    {State, Req3} = case cowboy_req:method(Req0) of
        {<<"PATCH">>, Req1} ->
            {ok, Body, Req2} = cowboy_req:body(Req1),
            case osc_http:parse_json_patch(Body) of
                {ok, {Operation, Path, Value}} ->
                    {#st{op=Operation, path=Path, value=Value}, Req2};
                false ->
                    {#st{}, Req2}
            end;
        {_Method, Req1} ->
            {#st{}, Req1}
    end,
    case cowboy_req:binding(org_id, Req3) of
        {undefined, Req4} ->
            {ok, Req4, State};
        {OrgIDBin, Req4} ->
            OrgID = list_to_integer(binary_to_list(OrgIDBin)),
            case osc_meta_org:lookup(OrgID) of
                not_found ->
                    {ok, Req4, State};
                {ok, OrgProps} ->
                    {ok, Req4, State#st{org_props=OrgProps}}
            end
    end.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PATCH">>], Req, State}.

is_authorized(Req, State) ->
    osc_http:is_authorized(
        Req,
        fun(UserID) -> State#st{user_id=UserID} end,
        fun() -> State end
    ).

forbidden(Req0, State) ->
    #st{
        org_props=OrgProps,
        user_id=UserID,
        op=Op,
        path=Path
    } = State,
    case OrgProps of
        undefined ->
            {false, Req0, State};
        OrgProps ->
            OrgID = proplists:get_value(id, OrgProps),
            {Method, Req1} = cowboy_req:method(Req0),
            Forbidden = case Method of
                <<"PATCH">> ->
                    %% Mutation requires ownership
                    IsOwner = osc_meta_org:is_owner(OrgID, UserID),
                    case {Op, Path} of
                        {<<"add">>, [<<"ports">>]} ->
                            Ports = proplists:get_value(ports, OrgProps),
                            length(Ports) > 1 orelse not IsOwner;
                        _ ->
                            not IsOwner
                    end;
                <<"GET">> ->
                    %% Viewing only requires membership
                    IsMember = osc_meta_org:is_member(OrgID, UserID),
                    not IsMember
            end,
            {Forbidden, Req1, State}
    end.

resource_exists(Req, #st{org_props=undefined}=State) ->
    {false, Req, State};
resource_exists(Req, State) ->
    {true, Req, State}.

content_types_accepted(Req, State) ->
    {
        [{{<<"application">>, <<"json-patch+json">>, '*'}, from_json_patch}],
        Req,
        State
    }.

from_json_patch(Req, State) ->
    #st{
        org_props=OrgProps0,
        op=Operation,
        path=Path,
        value=Value
    } = State,
    case apply_patch(Operation, Path, Value, OrgProps0) of
        {ok, OrgProps1} ->
            {true, Req, State#st{org_props=OrgProps1}};
        error ->
            {false, Req, State}
    end.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, #st{org_props=OrgProps}=State) ->
    Body = {[
        {id, proplists:get_value(id, OrgProps)},
        {name, proplists:get_value(name, OrgProps)},
        {ports, proplists:get_value(ports, OrgProps)}
    ]},
    {jiffy:encode(Body), Req, State}.

apply_patch(<<"add">>, [<<"ports">>], null, OrgProps) ->
    {ok, Port} = osc_meta_util:available_port(),
    ok = osc_meta_org:add_port(proplists:get_value(id, OrgProps), Port),
    Ports = [Port|proplists:get_value(ports, OrgProps)],
    {ok, [{ports, Ports}|proplists:delete(ports, OrgProps)]};
apply_patch(<<"remove">>, [<<"ports">>], Port, OrgProps) ->
    ok = osc_meta_org:remove_port(proplists:get_value(id, OrgProps), Port),
    Ports = lists:delete(Port, proplists:get_value(ports, OrgProps)),
    {ok, [{ports, Ports}|proplists:delete(ports, OrgProps)]};
apply_patch(Op, Path, _, OrgProps) ->
    lager:error(
        "Got an unknown patch attempt: ~p, ~p for org ~p",
        [Op, Path, OrgProps]
    ),
    error.
