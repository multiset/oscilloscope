-record(user, {
    id :: integer(),
    name :: binary(),
    password :: binary(),
    email :: binary(),
    created_at :: integer(),
    updated_at :: integer()
}).

-record(org, {
    id :: integer(),
    name :: binary()
}).

-record(team, {
    id :: integer(),
    org_id :: integer(),
    name :: binary()
}).
