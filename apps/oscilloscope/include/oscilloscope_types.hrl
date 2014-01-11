-type user() :: binary().
-type userid() :: integer().
-type host() :: binary().
-type service() :: binary().
-type group() :: {user(), service(), host()}.

-type aggregation() :: atom().

-type resolution_id() :: pos_integer().
-type interval() :: pos_integer().
-type count() :: pos_integer().
-type timestamp() :: pos_integer().
-type persisted() :: [{timestamp(), pos_integer()}].
-type resolution() :: {resolution_id(), interval(), count()}.

%% TODO: import proper type from supervisor
-type child_spec() :: any().
