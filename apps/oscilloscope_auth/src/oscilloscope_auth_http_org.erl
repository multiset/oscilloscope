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
    case oscilloscope_auth_util:get_authorized_user(ReqData) of
        #user{}=User ->
            {true, ReqData, User#user.id};
        Unauthorized ->
            {Unauthorized, ReqData, Context}
    end.

allowed_methods(ReqData, Context) ->
    {['PUT'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

to_json(ReqData, UserID) ->
    OrgName = wrq:path_info(org_name, ReqData),
    % TODO: Handle org creation errors
    {ok, Org} = oscilloscope_auth_org:create(OrgName),
    ok = oscilloscope_auth_team:add_members(
        Org#org.id,
        <<"owners">>,
        UserID
    ),
    {<<"{\"ok\": true}">>, ReqData, ok}.
