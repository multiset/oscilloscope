-type owner_id() :: pos_integer().
-type metric() :: {owner_id(), [{binary(), binary()}]}.
-type metric_id() :: pos_integer().

-type aggregation() :: atom().

-type resolution_id() :: pos_integer().
-type interval() :: pos_integer().
-type count() :: pos_integer().
-type timestamp() :: pos_integer().
-type persisted() :: [{timestamp(), pos_integer()}].
-type resolution() :: {resolution_id(), interval(), count(), persisted()}.

-type read() :: {timestamp(), timestamp(), resolution(), [number()]}.

%% TODO: import proper type from supervisor
-type child_spec() :: any().
