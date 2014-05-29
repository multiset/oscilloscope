-module(oscilloscope_auth_owner).

-export([create_port/2]).

-include("oscilloscope_auth.hrl").


-spec create_port(integer(), integer()) -> ok | {error, binary()}.
create_port(_OwnerID, _Port) ->
    % TODO: Create via curling external process
    ok.
