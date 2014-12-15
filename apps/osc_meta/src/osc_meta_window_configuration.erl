-module(osc_meta_window_configuration).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    create/4,
    list/1,
    lookup/1,
    delete/1,
    add_window/2,
    for_metric/1
]).

-include_lib("osc/include/osc_types.hrl").
-include_lib("osc_meta/include/osc_meta.hrl").

-spec create(OwnerID, Priority, GroupProps, WindowConfig) -> {ok, GroupID} when
    OwnerID :: owner_id(),
    Priority :: pos_integer(),
    GroupProps :: [{GroupTagKey, GroupTagValue}],
    GroupTagKey :: group_tag_key(),
    GroupTagValue :: group_tag_value(),
    WindowConfig :: [window_config()],
    GroupID :: group_id().

create(OwnerID, Priority, GroupProps, WindowConfigs) ->
    GroupSQL = "INSERT INTO window_configuration_groups"
               "(owner_id, priority, tags) VALUES ($1, $2, $3)"
               "RETURNING id;",
    Tags = lists:map(fun tuple_to_list/1, GroupProps),
    {ok, _, _, [{GroupID}]} = osc_sql:adhoc(
        GroupSQL,
        [OwnerID, Priority, Tags]
    ),
    WindowSQL = "INSERT INTO window_configurations"
                "(group_id, type, aggregation, interval, count)"
                "VALUES ($1, $2, $3, $4, $5);",
    ok = lists:foreach(
        fun({Type, Aggregation, Interval, Count}) ->
            Args = [
                GroupID,
                term_to_binary(Type),
                term_to_binary(Aggregation),
                Interval,
                Count
            ],
            {ok, _} = osc_sql:adhoc(WindowSQL, Args)
        end,
        WindowConfigs
    ),
    {ok, GroupID}.


-spec list(OwnerID) -> {ok, WindowConfigs} when
    OwnerID :: owner_id(),
    WindowConfigs :: proplists:proplist().

list(OwnerID) ->
    SQL = "SELECT groups.id, groups.priority, groups.tags,"
          " windows.type, windows.aggregation, windows.interval, windows.count"
          " FROM window_configuration_groups AS groups,"
          " window_configurations AS windows"
          " WHERE owner_id = $1 AND windows.group_id = groups.id;",
    {ok, _, Rows} = osc_sql:adhoc(SQL, [OwnerID]),
    %% Aggregate window configurations by group
    GroupedConfigs = lists:foldl(
        fun({GroupID, Priority, Tags, Type, Agg, Interval, Count}, Acc0) ->
            Window = [
                {type, binary_to_term(Type)},
                {aggregation, binary_to_term(Agg)},
                {interval, Interval},
                {count, Count}
            ],
            case lists:keytake(GroupID, 1, Acc0) of
                false ->
                    Value = [
                        {id, GroupID},
                        {priority, Priority},
                        {tags, lists:map(fun list_to_tuple/1, Tags)},
                        {windows, [Window]}
                    ],
                    [{GroupID, Value}|Acc0];
                {value, {GroupID, Value0}, Acc1} ->
                    {value, {windows, Windows}, Value1} = lists:keytake(
                        windows,
                        1,
                        Value0
                    ),
                    [{GroupID, [{windows, [Window|Windows]}|Value1]}|Acc1]
            end
        end,
        [],
        Rows
    ),
    Configs = [Props || {_ID, Props} <- GroupedConfigs],
    %% Sort by descending priority
    SortedConfigs = lists:sort(
        fun(PropsA, PropsB) ->
            PriorityA = proplists:get_value(priority, PropsA),
            PriorityB = proplists:get_value(priority, PropsB),
            PriorityA =< PriorityB
        end,
        Configs
    ),
    {ok, SortedConfigs}.


-spec lookup(GroupID) -> {ok, any()} | not_found when
    GroupID :: group_id().

lookup(GroupID) ->
    SQL = "SELECT groups.owner_id, groups.priority, groups.tags,"
          " windows.type, windows.aggregation, windows.interval, windows.count"
          " FROM window_configuration_groups AS groups,"
          " window_configurations AS windows"
          " WHERE windows.group_id = groups.id AND groups.id = $1;",
    {ok, _, Rows} = osc_sql:adhoc(SQL, [GroupID]),
    case Rows of
        [] ->
            not_found;
        [{OwnerID, Priority, Tags, _, _, _, _}|_] ->
            Windows = lists:map(
                fun({_, _, _, Type, Agg, Interval, Count}) ->
                    [
                        {type, binary_to_term(Type)},
                        {aggregation, binary_to_term(Agg)},
                        {interval, Interval},
                        {count, Count}
                    ]
                end,
                Rows
            ),
            {ok, [
                {id, GroupID},
                {owner_id, OwnerID},
                {priority, Priority},
                {tags, lists:map(fun list_to_tuple/1, Tags)},
                {windows, Windows}
            ]}
    end.


-spec delete(GroupID) -> ok | error when
    GroupID :: group_id().

delete(GroupID) ->
    Commands = [
        {delete_window_configurations, [GroupID]},
        {delete_window_group_configurations, [GroupID]}
    ],
    Batch = osc_sql:batch(Commands),
    case lists:usort([Status || {Status, _} <- Batch]) of
        [ok] ->
            ok;
        _ ->
            lager:error(
                "Window configuration deletion encountered an error: ~p",
                [Batch]
            ),
            error
    end.

-spec add_window(GroupID, WindowConfig) -> ok when
    GroupID :: group_id(),
    WindowConfig :: window_config().

add_window(GroupID, WindowConfig) ->
    {
        Type,
        Aggregation,
        Interval,
        Count
    } = WindowConfig,
    SQL = " INSERT INTO window_configurations"
          " (group_id, type, aggregation, interval, count)"
          " VALUES ($1, $2, $3, $4, $5);",
    Args = [
        GroupID,
        term_to_binary(Type),
        term_to_binary(Aggregation),
        Interval,
        Count
    ],
    {ok, 1} = osc_sql:adhoc(SQL, Args),
    ok.

-spec for_metric(Metric) -> {ok, Windows} when
    Metric :: metric(),
    Windows :: [window_config()].

for_metric({OwnerID, Props}) ->
    SQL = <<
        "SELECT id, tags"
        " FROM window_configuration_groups"
        " WHERE owner_id=$1"
        " ORDER BY priority DESC;"
    >>,
    {ok, _, Rows0} = osc_sql:adhoc(SQL, [OwnerID]),
    %% Vector types come back as lists of lists, and that's a pain.
    Rows1 = lists:map(
        fun({ID, Tags}) -> {ID, lists:map(fun list_to_tuple/1, Tags)} end,
        Rows0
    ),
    find_configuration_match(Rows1, Props).


-spec find_configuration_match(GroupSpecs, MetricProps) -> {ok, [Config]} when
    GroupSpecs :: [{GroupID, [{GroupTagKey, GroupTagValue}]}],
    GroupID :: group_id(),
    GroupTagKey :: group_tag_key(),
    GroupTagValue :: group_tag_value(),
    MetricProps :: [{prop_key(), prop_value()}],
    Config :: window_config().

find_configuration_match([], _) ->
    application:get_env(
        osc_meta,
        default_window_configuration
    );
find_configuration_match([{GroupID, KVs}|Groups], Props) ->
    %% Find the highest-priority group that matches. All *group* properties must
    %% be represented in the metric props, but not all metric properties must be
    %% represented in a group in order to be considered a match.
    case match_props(KVs, Props) of
        true ->
            SQL = "SELECT type, aggregation, interval, count"
                  " FROM window_configurations"
                  " WHERE group_id = $1;",
            {ok, _,  Rows} = osc_sql:adhoc(SQL, [GroupID]),
            Windows = lists:map(
                fun({Type, Aggregation, Interval, Count}) ->
                    {
                        binary_to_term(Type),
                        binary_to_term(Aggregation),
                        Interval,
                        Count
                    }
                end,
                Rows
            ),
            {ok, Windows};
        false ->
            find_configuration_match(Groups, Props)
    end.


-spec match_props(GroupSpecs, MetricProps) -> Match when
    GroupSpecs :: [{GroupID, [{GroupTagKey, GroupTagValue}]}],
    GroupID :: group_id(),
    GroupTagKey :: group_tag_key(),
    GroupTagValue :: group_tag_value(),
    MetricProps :: [{prop_key(), prop_value()}],
    Match :: boolean().

match_props([], _) ->
    true;
match_props([{GroupKey, GroupValue}|KVs], Props) ->
    case proplists:get_value(GroupKey, Props) of
        undefined ->
            false;
        PropValue ->
            Match = re:run(PropValue, GroupValue) =/= nomatch,
            case Match of
                true -> match_props(KVs, Props);
                false -> false
            end
    end.

