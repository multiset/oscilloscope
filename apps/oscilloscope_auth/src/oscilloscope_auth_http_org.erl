-module(oscilloscope_auth_http_org).

-include("oscilloscope_auth.hrl").

-export([
    init/1,
    is_authorized/2,
    allowed_methods/2,
    content_types_provided/2,
    to_json/2
]).

init([]) ->
    {ok, ok}.

is_authorized(ReqData, Context) ->
    oscilloscope_auth_util:is_authorized(ReqData, Context).

allowed_methods(ReqData, Context) ->
    {['PUT'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
    OrgName = wrq:path_info(org_name, ReqData),
    {ok, _} = oscilloscope_auth_org:create(OrgName),
    {<<"{\"ok\": true}">>, ReqData, Context}.
