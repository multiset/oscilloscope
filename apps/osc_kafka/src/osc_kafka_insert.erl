-module(osc_kafka_insert).

-export([
    start_link/1,
    insert/1
]).

start_link(Points) ->
    {ok, spawn_link(fun() -> insert(Points) end)}.


insert([]) ->
    ok;
insert([{MetricID, Time, Value}|Ps]=Points) when is_integer(MetricID) ->
    case osc_cache:find(MetricID) of
        {ok, Pid} ->
            ok = osc_cache:update(Pid, [{Time, Value}]),
            insert(Ps);
        not_found ->
            case osc_cache:start(MetricID) of
                {ok, Pid} ->
                    ok = osc_cache:update(Pid, [{Time, Value}]),
                    insert(Ps);
                {error, _} ->
                    insert(Points)
            end
    end;
insert([{Metric, Time, Value}|Ps]) ->
    {ok, Pid} = case osc_cache:find(Metric) of
        {ok, Pid0} ->
            {ok, Pid0};
        not_found ->
            osc_kafka_metric_creator_sup:start_child(Metric)
    end,
    ok = osc_cache:update(Pid, [{Time, Value}]),
    insert(Ps).

