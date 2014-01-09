-module(oscilloscope_http_metric).

-export([
    init/1,
    ping/2,
    content_types_provided/2,
    allowed_methods/2,
    to_json/2
]).

-record(st, {

}).

init([]) ->
    {ok, #st{}}.

ping(Req, State) ->
    {pong, Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

to_json(Req, State) ->
    Metric = list_to_binary(wrq:path_info(metric_name, Req)),
    From = list_to_integer(wrq:get_qs_value("from", Req)),
    Until = list_to_integer(wrq:get_qs_value("until", Req)),
    {ok, Values} = oscilloscope_cache:read(Metric, From, Until),
    {jiffy:encode({Values}), Req, State}.
