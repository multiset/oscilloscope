-module(oscilloscope_auth_http_rules).

-include("oscilloscope_auth.hrl").

-export([
    init/1,
    is_authorized/2,
    malformed_request/2,
    allowed_methods/2,
    content_types_provided/2,
    to_json/2
]).

-record(state, {
    org,
    team,
    tags,
    level
}).

init([]) ->
    {ok, #state{}}.

allowed_methods(ReqData, State) ->
    {['PUT', 'DELETE'], ReqData, State}.

is_authorized(ReqData, State) ->
    OrgNameOrID = wrq:path_info(org_name_or_id, ReqData),
    case oscilloscope_auth_util:get_authorized_user(ReqData) of
        #user{}=User ->
            case oscilloscope_auth_org:lookup(OrgNameOrID) of
                #org{}=Org ->
                    Authed = oscilloscope_auth_org:is_owner(User, Org),
                    {Authed, ReqData, State#state{org=Org}};
                not_found ->
                    {false, ReqData, State}
            end;
        Unauthorized ->
           {Unauthorized, ReqData, State}
    end.

malformed_request(ReqData, State) ->
    LevelBin = wrq:path_info(level, ReqData),
    try binary_to_integer(LevelBin) of
        Level ->
            case {wrq:method(ReqData), wrq:req_body(ReqData)} of
                {'GET', _} ->
                    {false, ReqData, State};
                {'POST', undefined} ->
                    {true, ReqData, State};
                {'POST', Body} ->
                    case oscilloscope_auth_util:parse_body(Body) of
                        false ->
                            {true, ReqData, State};
                        ParsedBody ->
                            State1 = State#state{
                                tags=ParsedBody,
                                level=Level
                            },
                            {false, ReqData, State1}
                    end
            end
    catch error:badarg ->
        {true, ReqData, State}
    end.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    #state{
        org=Org,
        team=Team,
        tags=Tags,
        level=Level
    } = State,
    ok = oscilloscope_auth_rules:grant(Org#org.id, Team#team.id, Tags, Level),
    {<<"{\"ok\": true}">>, ReqData, State}.
