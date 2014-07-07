-module(oscilloscope_auth_http_org).

-include("oscilloscope_auth.hrl").

-export([
    init/1,
    ping/2,
    is_authorized/2,
    forbidden/2,
    malformed_request/2,
    allowed_methods/2,
    content_types_accepted/2,
    to_json/2
]).

-record(state, {
    org_name,
    user
}).

init([]) ->
    {ok, #state{}}.

ping(ReqData, State) ->
    {pong, ReqData, State}.

is_authorized(ReqData, State) ->
    case oscilloscope_auth_util:get_authorized_user(ReqData) of
        #user{}=User ->
            {true, ReqData, State#state{user=User}};
        Unauthorized ->
            {Unauthorized, ReqData, State}
    end.

forbidden(ReqData, State) ->
    % TODO: Limit number of orgs a person can create or something
    {false, ReqData, State}.

allowed_methods(ReqData, State) ->
    {['PUT'], ReqData, State}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

malformed_request(ReqData, State) ->
    OrgName = wrq:path_info(org_name, ReqData),
    Malformed = not is_valid_org_name(OrgName),
    {Malformed, ReqData, State#state{org_name=OrgName}}.

to_json(ReqData, State) ->
    #state{user=User, org_name=OrgName} = State,
    % TODO: Handle org creation errors
    {ok, Org} = oscilloscope_auth_org:create(OrgName),
    ok = oscilloscope_auth_org:add_member(Org, User),
    {ok, Owners} = case oscilloscope_auth_team:find(Org, <<"owners">>) of
        not_found ->
            oscilloscope_auth_team:create(Org, <<"owners">>);
        {ok, #team{}}=Result ->
            Result
    end,
    ok = oscilloscope_auth_team:add_member(Org, Owners, User),
    {true, wrq:set_resp_body(<<"{\"ok\": true}">>, ReqData), ok}.

is_valid_org_name(_) ->
    % TODO
    true.
