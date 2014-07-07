-module(oscilloscope_auth_http_user).

-include("oscilloscope_auth.hrl").

-export([
    init/1,
    ping/2,
    allowed_methods/2,
    malformed_request/2,
    content_types_accepted/2,
    from_json/2
]).

-record(state, {
    name,
    email,
    pass
}).

init([]) ->
    {ok, #state{}}.

ping(ReqData, State) ->
    {pong, ReqData, State}.

allowed_methods(ReqData, State) ->
    {['PUT'], ReqData, State}.

malformed_request(ReqData, State) ->
    case wrq:req_body(ReqData) of
        undefined ->
            {true, ReqData, State};
        Body ->
            try jiffy:decode(Body) of
                {ParsedBody} ->
                    MaybePass = lists:keyfind(<<"password">>, 1, ParsedBody),
                    MaybeEmail = lists:keyfind(<<"email">>, 1, ParsedBody),
                    case {MaybePass, MaybeEmail} of
                        {{<<"password">>, Pass}, {<<"email">>, Email}} ->
                            ListName = wrq:path_info(username, ReqData),
                            Name = list_to_binary(ListName),
                            State1 = State#state{
                                pass=Pass,
                                name=Name,
                                email=Email
                            },
                            InvalidEmail = not is_valid_email(Email),
                            InvalidName = not is_valid_name(Name),
                            {InvalidEmail orelse InvalidName, ReqData, State1};
                        _ ->
                            {true, ReqData, State}
                    end;
                _Other ->
                    {true, ReqData, State}
            catch throw:_Reason ->
                {true, ReqData, State}
            end
    end.

is_valid_name(_) ->
    % whee
    true.

is_valid_email(_) ->
    % Ha
    true.

content_types_accepted(ReqData, State) ->
    {[{"application/json", from_json}], ReqData, State}.

from_json(ReqData, State) ->
    #state{name=Name, email=Email, pass=Pass} = State,
    {ok, _User} = oscilloscope_auth_user:create(Name, Email, Pass),
    {true, wrq:set_resp_body(<<"{\"ok\": true}">>, ReqData), ok}.
