-module(oscilloscope_auth_http_metrics).

-include("oscilloscope_auth.hrl").

-export([
    init/1,
    ping/2,
    is_authorized/2,
    malformed_request/2,
    allowed_methods/2,
    content_types_accepted/2,
    from_form/2
]).

-record(state, {
    user,
    tags
}).

init([]) ->
    {ok, #state{}}.

ping(ReqData, State) ->
    {true, ReqData, State}.

allowed_methods(ReqData, State) ->
    {['GET', 'POST'], ReqData, State}.

is_authorized(ReqData, State) ->
    case oscilloscope_auth_util:get_authorized_user(ReqData) of
        #user{}=User ->
            {true, ReqData, State#state{user=User}};
        Unauthorized ->
            {Unauthorized, ReqData, State}
    end.

malformed_request(ReqData, State) ->
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
                    {false, ReqData, State#state{tags=ParsedBody}}
            end
    end.

content_types_accepted(ReqData, State) ->
    {[{"application/x-www-form-urlencoded", from_form}], ReqData, State}.

from_form(ReqData, State) ->
    {<<"">>, ReqData, State}.
