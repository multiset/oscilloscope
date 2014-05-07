-module(oscilloscope_http_find_metric).

-export([
    init/1,
    ping/2,
    malformed_request/2,
    allowed_methods/2,
    content_types_provided/2,
    to_json/2
]).

-record(st, {
    'query'
}).

init([]) ->
    {ok, #st{}}.

ping(Req, State) ->
    {pong, Req, State}.

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

to_json(Req, #st{query=Query}=State) ->
    % TODO: Add metric-finding capability
    {jiffy:encode({[]}), Req, State}.
