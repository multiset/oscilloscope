-module(oscilloscope_http_metric).

-export([
    init/1,
    ping/2,
    malformed_request/2,
    content_types_provided/2,
    allowed_methods/2,
    to_json/2
]).

-record(st, {
    metric_name,
    from,
    until
}).

init([]) ->
    {ok, #st{}}.

ping(Req, State) ->
    {pong, Req, State}.

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
    Metric = list_to_binary(State#st.metric_name),
    From = list_to_integer(State#st.from),
    Until = list_to_integer(State#st.until),
    %% TODO: handle timeout
    %% TODO: dereference Metric in to proper IDs
    {ok, Values} = oscilloscope:read(Metric, From, Until),
    {jiffy:encode({Values}), Req, State}.
