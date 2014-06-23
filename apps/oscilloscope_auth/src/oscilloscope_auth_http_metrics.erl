-module(oscilloscope_auth_http_metrics).

-include("oscilloscope_auth.hrl").

-export([
    init/1,
    ping/2,
    is_authorized/2,
    resource_exists/2,
    malformed_request/2,
    allowed_methods/2,
    content_types_accepted/2,
    from_form/2
]).

-record(state, {
    type,
    user,
    org,
    tags
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
    #state{org=Org, type=Type} = State,
    case oscilloscope_auth_util:get_authorized_user(ReqData) of
        #user{}=User ->
            IsAuthed = oscilloscope_auth_org:is_member(Org, User),
            State1 = case Type of
                org ->
                    State#state{user=User};
                user ->
                    State
            end,
            {IsAuthed, ReqData, State1};
        Unauthorized ->
            {Unauthorized, ReqData, State}
    end.

malformed_request(ReqData, State) ->
    case {wrq:method(ReqData), wrq:req_body(ReqData)} of
        {'POST', undefined} ->
            {true, ReqData, State};
        {'POST', Body} ->
            try jiffy:decode(Body) of
                ParsedBody when is_list(ParsedBody) ->
                    % Check that all elements in body are {binary(), binary()}
                    IsMalformed = lists:all(fun(KV) ->
                        case KV of
                            {K, V} when is_binary(K) andalso is_binary(V) ->
                                true;
                            _ ->
                                false
                        end
                    end, ParsedBody),
                    {IsMalformed, ReqData, State#state{tags=ParsedBody}};
                false ->
                    {true, ReqData, State}
            catch throw:_ ->
                {true, ReqData, State}
            end
    end.

content_types_accepted(ReqData, State) ->
    {[{"application/json", from_form}], ReqData, State}.

from_form(ReqData, State) ->
    #state{
        user=User,
        org=Org,
        tags=Tags
    } = State,
    Resp = oscilloscope_auth_metrics:find(Org#org.id, User#user.id, Tags),
    RespJSON = jiffy:encode(Resp),
    {true, wrq:set_resp_body(RespJSON, ReqData), State}.
