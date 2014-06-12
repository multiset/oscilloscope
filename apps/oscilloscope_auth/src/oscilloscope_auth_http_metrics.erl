-module(oscilloscope_auth_http_metrics).

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
    user,
    args
}).

init([]) ->
    {ok, #state{}}.

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
                    {false, ReqData, State#state{args=ParsedBody}}
            end
    end.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    {<<"">>, ReqData, State}.
