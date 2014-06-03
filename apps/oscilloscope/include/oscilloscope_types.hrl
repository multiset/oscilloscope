-type owner_id() :: pos_integer().
-type metric() :: {owner_id(), [{binary(), binary()}]}.
-type metric_id() :: pos_integer().

-type aggregation() :: atom().

-type resolution_id() :: pos_integer().
-type interval() :: pos_integer().
-type count() :: pos_integer().
-type timestamp() :: pos_integer().
-type value() :: nonempty_list(number()) | null.
-type persisted() :: [{timestamp(), pos_integer()}].

-type read() :: {
    timestamp(),
    timestamp(),
    [value()]
}.

%% TODO: import proper type from supervisor
-type child_spec() :: any().
