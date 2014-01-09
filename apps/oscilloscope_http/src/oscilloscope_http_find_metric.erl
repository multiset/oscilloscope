-module(oscilloscope_http_find_metric).

-export([
    init/1,
    ping/2,
    malformed_request/2,
    allowed_methods/2,
    content_types_provided/2,
    is_authorized/2,
    to_json/2
]).

-record(st, {
    'query',
    user
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
    case wrq:get_qs_value("query", Req) of
        undefined ->
            {true, Req, State};
        Query ->
            {false, Req, State#st{'query'=list_to_binary(Query)}}
    end.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

to_json(Req, State) ->
    #st{user=User, 'query'=Query} = State,
    {ok, Metrics} = oscilloscope_sql_util:find_metrics(User#user.id, Query),
    {jiffy:encode(Metrics), Req, State}.
