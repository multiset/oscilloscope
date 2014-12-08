-module(osc_meta_org_handler).

-export([init/3, terminate/3]).
-export([
    rest_init/2,
    allowed_methods/2,
    resource_exists/2,
    content_types_accepted/2,
    content_types_provided/2,
    from_json_patch/2,
    to_json/2
]).

-record(st, {
    org_props
}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
    ok.

rest_init(Req, _State) ->
    {ok, Req, #st{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PATCH">>], Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:binding(orgid, Req) of
        {undefined, Req1} ->
            {false, Req1, State};
        {OrgIDBin, Req1} ->
            OrgID = list_to_integer(binary_to_list(OrgIDBin)),
            case osc_meta_org:lookup(OrgID) of
                not_found ->
                    lager:error("Org not found"),
                    {false, Req1, State};
                {ok, OrgProps} ->
                    {true, Req1, State#st{org_props=OrgProps}}
            end
    end.

content_types_accepted(Req, State) ->
    {
        [{{<<"application">>, <<"json-patch+json">>, '*'}, from_json_patch}],
        Req,
        State
    }.

from_json_patch(Req0, #st{org_props=OrgProps0}=State) ->
    {ok, Body, Req1} = cowboy_req:body(Req0),
    {ok, {Operation, Path, Value}} = osc_http:parse_json_patch(Body),
    case apply_patch(Operation, Path, Value, OrgProps0) of
        {ok, OrgProps1} ->
            {true, Req1, State#st{org_props=OrgProps1}};
        error ->
            {false, Req1, State}
    end.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, #st{org_props=OrgProps}=State) ->
    Body = {[
        {id, proplists:get_value(id, OrgProps)},
        {name, proplists:get_value(name, OrgProps)}
    ]},
    {jiffy:encode(Body), Req, State}.

apply_patch(Op, Path, _, OrgProps) ->
    lager:error(
        "Got an unknown patch attempt: ~p, ~p for org ~p",
        [Op, Path, OrgProps]
    ),
    error.
