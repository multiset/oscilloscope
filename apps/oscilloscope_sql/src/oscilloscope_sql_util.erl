-module(oscilloscope_sql_util).

-export([find_metrics/2]).

% Rewriting this terrible function requires a non-terrible SQL index.
find_metrics(UserID, Query) ->
    QueryLen = byte_size(Query)-1,
    {ok, _, Resp} = oscilloscope_sql:named(find_metrics, [UserID, Query]),
    Fun = fun([Leaf]) -> [leaf, Leaf]; ([Branch|_]) -> [branch, Branch] end,
    A = [binary:split(binary:part(Name, {byte_size(Name), QueryLen-byte_size(Name)}), <<".">>) || {Name} <- Resp],
    {ok, lists:usort(lists:map(Fun, A))}.
