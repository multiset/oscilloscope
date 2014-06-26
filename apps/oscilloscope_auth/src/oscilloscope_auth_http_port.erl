-module(oscilloscope_auth_http_port).

-include("oscilloscope_auth.hrl").

-export([
    init/1,
    ping/2,
    is_authorized/2,
    resource_exists/2,
    forbidden/2,
    allowed_methods/2,
    content_types_provided/2,
    to_json/2
]).

-record(state, {
    type,
    org,
    user
}).

init([Type]) ->
    {ok, #state{type=Type}}.

ping(ReqData, State) ->
    {true, ReqData, State}.

allowed_methods(ReqData, State) ->
    {['POST'], ReqData, State}.

resource_exists(ReqData, State) ->
    case State#state.type of
        user ->
            UserName = wrq:path_info(username, ReqData),
            case oscilloscope_auth_user:lookup(UserName) of
                not_found ->
                    {false, ReqData, State};
                #user{}=User ->
                    {true, ReqData, State#state{user=User}}
            end;
        org ->
            OrgName = wrq:path_info(orgname, ReqData),
            case oscilloscope_auth_org:lookup(OrgName) of
                not_found ->
                    {false, ReqData, State};
                #org{}=Org ->
                    {true, ReqData, State#state{org=Org}}
            end
    end.

is_authorized(ReqData, State) ->
    #state{org=Org, user=RequestedUser, type=Type} = State,
    case oscilloscope_auth_util:get_authorized_user(ReqData) of
        #user{}=User ->
            IsAuthed = case Type of
                org ->
                    oscilloscope_auth_org:is_owner(Org, User);
                user ->
                    RequestedUser#user.id =:= User#user.id
            end,
            {IsAuthed, ReqData, State#state{user=User}};
        Unauthorized ->
            {Unauthorized, ReqData, State}
    end.

forbidden(ReqData, State) ->
    % TODO: Limit the number of ports a person can create
    {false, ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    #state{type=Type, org=Org, user=User} = State,
    OwnerID = case Type of
        org ->
            Org#org.owner_id;
        user ->
            User#user.owner_id
    end,
    Resp = oscilloscope_auth_port:create(OwnerID),
    RespJSON = jiffy:encode({[Resp]}),
    {true, wrq:set_resp_body(RespJSON, ReqData), State}.
