-record(metric, {
    key,
    aggregation
}).

-record(resolution, {
    key,
    interval,
    count
}).

-record(persist, {
    key,
    timestamp,
    count
}).
