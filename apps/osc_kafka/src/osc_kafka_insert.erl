-module(osc_kafka_insert).

-export([
    start_link/1,
    insert/1
]).

start_link(Points) ->
    {ok, spawn_link(fun() -> insert(Points) end)}.


insert(Points) ->
    lists:foreach(fun({Metric, Time, Value}) ->
        {ok, Pid} = case osc_cache:find(Metric) of
            {ok, Pid0} ->
                {ok, Pid0};
            not_found ->
                case osc_cache:start(Metric) of
                    {ok, Pid1} ->
                        {ok, Pid1};
                    not_found ->
                        osc_kafka_metric_creator_sup:start_child(Metric)
                end
        end,
        ok = osc_cache:update(Pid, [{Time, Value}])
    end, Points).
