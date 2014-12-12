-type metric_id() :: pos_integer().

-type aggregation() :: atom().

-type interval() :: pos_integer().
-type count() :: pos_integer().
-type timestamp() :: pos_integer().
-type wrapped_value() :: nonempty_list(number()) | null.
-type value() :: number() | null.
-type persisted() :: [{timestamp(), pos_integer()}].

-type org_id() :: pos_integer().
-type user_id() :: pos_integer().
-type team_id() :: pos_integer().
