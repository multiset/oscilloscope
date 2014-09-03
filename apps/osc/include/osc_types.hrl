-type owner_id() :: pos_integer().
-type metric() :: {owner_id(), [{binary(), binary()}]}.
-type metric_id() :: pos_integer().

-type aggregation() :: atom().

-type resolution_id() :: pos_integer().
-type interval() :: pos_integer().
-type count() :: pos_integer().
-type timestamp() :: pos_integer().
-type wrapped_value() :: nonempty_list(number()) | null.
-type value() :: number() | null.
-type persisted() :: [{timestamp(), pos_integer()}].

-type read() :: {timestamp(), timestamp(), [value()]} | not_found.

-type cache_read() :: {
    osc_metadata:meta(),
    osc_metadata_resolution:resolution(),
    read()
}.
