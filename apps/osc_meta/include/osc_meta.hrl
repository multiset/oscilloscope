-type prop_key() :: binary().
-type prop_value() :: binary().
-type metric() :: {org_id(), [{prop_key(), prop_value()}]}.

-type group_id() :: pos_integer().
-type group_tag_key() :: binary().
-type group_tag_value() :: binary().


-type window_config() :: {
    osc_meta_window:window_id(),
    osc_meta_window:window_type(),
    aggregation(),
    interval(),
    count()
}.

-record(metricmeta, {
    id :: metric_id(),
    org_id :: org_id(),
    props :: any(), %% TODO
    encoded_props :: binary(),
    windows :: [osc_meta_window:windowmeta()]
}).

-type metricmeta() :: #metricmeta{}.

