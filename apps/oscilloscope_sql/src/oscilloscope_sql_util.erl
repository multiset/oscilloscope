-module(oscilloscope_sql_util).

-export([get_user_id/1, find_metrics/2]).
-export([to_plists/1]).

get_user_id(Username) ->
    Resp = oscilloscope_sql:named(select_user_id, [Username]),
    case to_plists(Resp) of
        {ok, Plist} ->
            [{"id", UserID}] = Plist,
            UserID;
        {error, _} ->
            % TODO: Check error message
            throw(nouser)
    end.

% Rewriting this terrible function requires a non-terrible SQL index.
find_metrics(UserID, Query) ->
    QueryLen = byte_size(Query)-1,
    {ok, _, Resp} = oscilloscope_sql:named(find_metrics, [UserID, Query]),
    Fun = fun([Leaf]) -> [leaf, Leaf]; ([Branch|_]) -> [branch, Branch] end,
    A = [binary:split(binary:part(Name, {byte_size(Name), QueryLen-byte_size(Name)}), <<".">>) || {Name} <- Resp],
    {ok, lists:usort(lists:map(Fun, A))}.

to_plists({ok, Columns0, Resp}) ->
    Columns = [element(1, Column) || Column <- Columns0],
    ZipFun = fun(Row) -> lists:zip(Columns, tuple_to_list(Row)) end,
    {ok, lists:map(ZipFun, Resp)};
to_plists(Other) ->
    {error, Other}.
