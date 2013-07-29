-module(oscilloscope_query_metric).

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
    {jiffy:encode({[{foo, [Metric, 2.3, true]}]}), Req, State}.
