-module(oscilloscope_http_metric).

-export([
    init/1,
    ping/2,
    malformed_request/2,
    is_authorized/2,
    content_types_provided/2,
    allowed_methods/2,
    to_json/2
]).

-record(st, {
    user,
    metric_name,
    from,
    until
}).

-include("oscilloscope_http.hrl").

init([]) ->
    {ok, #st{}}.

ping(Req, State) ->
    {pong, Req, State}.

is_authorized(Req, State) ->
    case oscilloscope_http_auth:get_authorized_user(Req) of
        #user{}=User ->
            {true, Req, State#st{user=User}};
        Other ->
            {Other, Req, State}
    end.

malformed_request(Req, State) ->
    try
        Metric = oscilloscope_http_util:get_path_info(metric_name, Req),
        From = oscilloscope_http_util:get_qs("from", Req),
        Until = oscilloscope_http_util:get_qs("until", Req),
        {false, Req, State#st{metric_name=Metric, from=From, until=Until}}
    catch _ ->
        {true, Req, State}
    end.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

to_json(Req, State) ->
    User = State#st.user,
    Metric = list_to_binary(State#st.metric_name),
    From = list_to_integer(State#st.from),
    Until = list_to_integer(State#st.until),
    {ok, Values} = oscilloscope_cache:read(User#user.id, Metric, <<"">>, From, Until),
    {jiffy:encode({Values}), Req, State}.
