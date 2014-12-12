-type owner_id() :: pos_integer().
-type prop_key() :: binary().
-type prop_value() :: binary().
-type metric() :: {owner_id(), [{prop_key(), prop_value()}]}.

-type group_id() :: pos_integer().
-type group_tag_key() :: binary().
-type group_tag_value() :: binary().

-type window_id() :: pos_integer().
-type window_type() :: rectangular.
-type window_config() :: {window_type(), aggregation(), interval(), count()}.

-record(metricmeta, {
    id :: metric_id(),
    owner_id :: owner_id(),
    props :: any(), %% TODO
    encoded_props :: binary(),
    windows :: [osc_meta_window:windowmeta()]
}).

-record(windowmeta, {
    id :: window_id(),
    metric_id :: metric_id(),
    window_type :: window_type(),
    aggregation :: aggregation(),
    interval :: interval(),
    count :: count(),
    persisted :: persisted()
}).

-type metricmeta() :: #metricmeta{}.
-type windowmeta() :: #windowmeta{}.
