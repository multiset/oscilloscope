-type user() :: binary().
-type host() :: binary().
-type service() :: binary().
-type group() :: {user(), service(), host()}.

-type aggregation() :: atom().

-type resolution_id() :: pos_integer().
-type interval() :: pos_integer().
-type count() :: pos_integer().
-type timestamp() :: pos_integer().
-type persisted() :: [timestamp()].
-type resolution() :: {resolution_id(), interval(), count(), persisted()}.

%% TODO: import proper type from supervisor
-type child_spec() :: any().
