-record(user, {
    id :: integer(),
    owner_id :: integer(),
    password :: binary(),
    name :: binary(),
    email :: binary(),
    created_at :: integer(),
    updated_at :: integer(),
    orgs :: dict()
}).

-record(org, {
    id :: integer(),
    owner_id :: integer(),
    name :: binary(),
    teams :: [integer()],
    members :: [integer()]
}).

-record(team, {
    id :: integer(),
    org_id :: integer(),
    name :: binary(),
    members :: [integer()]
}).
