-module(oscilloscope_auth_http_user).

-include("oscilloscope_auth.hrl").

-export([
    init/1,
    allowed_methods/2,
    malformed_request/2,
    content_types_provided/2,
    to_json/2
]).

-record(state, {
    email,
    pass
}).

init([]) ->
    {ok, ok}.

allowed_methods(ReqData, State) ->
    {['PUT'], ReqData, State}.

malformed_request(ReqData, State) ->
    case wrq:req_body(ReqData) of
        undefined ->
            {true, ReqData, State};
        Body ->
            case oscilloscope_auth_util:parse_body(Body) of
                false ->
                    {true, ReqData, State};
                ParsedBody ->
                    case lists:keyfind(<<"password">>, 1, ParsedBody) of
                        false ->
                            {true, ReqData, State};
                        {_, Pass} ->
                            Email = wrq:path_info(email, ReqData),
                            State1 = State#state{pass=Pass, email=Email},
                            {not is_valid_email(Email), ReqData, State1}
                    end
            end
    end.

is_valid_email(_) ->
    % Ha
    true.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    oscilloscope_auth_user:create(State#state.email, State#state.pass),
    {<<"{\"ok\": true}">>, ReqData, ok}.
