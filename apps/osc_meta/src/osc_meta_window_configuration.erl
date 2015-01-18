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
    delete_window/2,
    set_priority/2,
    for_metric/1
]).

-include_lib("osc/include/osc_types.hrl").
-include_lib("osc_meta/include/osc_meta.hrl").

-spec create(OwnerID, Priority, GroupProps, WindowConfig) -> Response when
    OwnerID :: owner_id(),
    Priority :: pos_integer(),
    GroupProps :: [{GroupTagKey, GroupTagValue}],
    GroupTagKey :: group_tag_key(),
    GroupTagValue :: group_tag_value(),
    WindowConfig :: [window_config()],
    Response :: {ok, GroupID} | {error, Error},
    GroupID :: group_id(),
    Error :: exists.

create(OwnerID, Priority, GroupProps, WindowConfigs) ->
    GroupSQL = "INSERT INTO window_configuration_groups"
               "(owner_id, priority, tags) VALUES ($1, $2, $3)"
               "RETURNING id;",
    WindowSQL = "INSERT INTO window_configurations"
                "(group_id, type, aggregation, interval, count)"
                "VALUES ($1, $2, $3, $4, $5);",
    ok = mpgsql:tx_begin(),
    Tags = lists:map(fun tuple_to_list/1, GroupProps),
    case mpgsql:equery(GroupSQL, [OwnerID, Priority, Tags]) of
        {error, unique_violation} ->
            ok = mpgsql:tx_rollback(),
            {error, exists};
        {ok, _, _, [{GroupID}]} ->
            lists:foreach(
                fun({Type, Aggregation, Interval, Count}) ->
                    Args = [
                        GroupID,
                        term_to_binary(Type),
                        term_to_binary(Aggregation),
                        Interval,
                        Count
                    ],
                    {ok, _} = mpgsql:equery(WindowSQL, Args)
                end,
                WindowConfigs
            ),
            ok = mpgsql:tx_commit(),
            {ok, GroupID}
    end.


-spec list(OwnerID) -> {ok, WindowConfigs} when
    OwnerID :: owner_id(),
    WindowConfigs :: proplists:proplist().

list(OwnerID) ->
    SQL = "SELECT id, priority, tags FROM window_configuration_groups "
          "WHERE owner_id = $1;",
    {ok, _, Rows} = mpgsql:equery(SQL, [OwnerID]),
    Configs = lists:map(
        fun({GroupID, Priority, Tags}) ->
            format_group(GroupID, OwnerID, Priority, Tags)
        end,
        Rows
    ),
    SortedConfigs = lists:sort(
        fun(PropsA, PropsB) ->
            PriorityA = proplists:get_value(priority, PropsA),
            PriorityB = proplists:get_value(priority, PropsB),
            PriorityA >= PriorityB
        end,
        Configs
    ),
    {ok, SortedConfigs}.


-spec lookup(GroupID) -> {ok, any()} | not_found when
    GroupID :: group_id().

lookup(GroupID) ->
    SQL = "SELECT owner_id, priority, tags"
          "FROM window_configuration_groups"
          "WHERE id = $1;",
    {ok, _, Rows} = mpgsql:equery(SQL, [GroupID]),
    case Rows of
        [] ->
            not_found;
        [{OwnerID, Priority, Tags}] ->
            {ok, format_group(GroupID, OwnerID, Priority, Tags)}
    end.

-spec delete(GroupID) -> ok when
    GroupID :: group_id().

delete(GroupID) ->
    ok = mpgsql:tx_begin(),
    DeleteWindowGroupConfigSQL = "DELETE FROM window_configuration_groups "
                                 "WHERE id = $1;",
    DeleteWindowConfigSQL = "DELETE FROM window_configurations "
                            "WHERE group_id = $1;",
    {ok, _} = mpgsql:equery(DeleteWindowConfigSQL, [GroupID]),
    {ok, _} = mpgsql:equery(DeleteWindowGroupConfigSQL, [GroupID]),
    ok = mpgsql:tx_commit().


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
    {ok, 1} = mpgsql:equery(SQL, Args),
    ok.


-spec delete_window(GroupID, WindowID) -> ok when
    GroupID :: group_id(),
    WindowID :: osc_meta_window:window_id().

delete_window(GroupID, WindowID) ->
    SQL = "DELETE FROM window_configurations "
          "WHERE group_id = $1 AND id = $2;",
    {ok, 1} = mpgsql:equery(SQL, [GroupID, WindowID]),
    ok.


-spec set_priority(GroupID, Priority) -> ok when
    GroupID :: group_id(),
    Priority :: pos_integer().

set_priority(GroupID, Priority) ->
    SQL = "UPDATE window_configuration_groups "
          "SET priority = $2 "
          "WHERE id = $1;",
    {ok, 1} = mpgsql:equery(SQL, [GroupID, Priority]),
    ok.


-spec for_metric(Metric) -> {ok, Windows} when
    Metric :: metric(),
    Windows :: [window_config()].

for_metric({OwnerID, Props}) ->
    SQL = "SELECT id, tags "
          "FROM window_configuration_groups "
          "WHERE owner_id = $1 "
          "ORDER BY priority DESC;",
    {ok, _, Rows0} = mpgsql:equery(SQL, [OwnerID]),
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
            SQL = "SELECT type, aggregation, interval, count "
                  "FROM window_configurations "
                  "WHERE group_id = $1;",
            {ok, _,  Rows} = mpgsql:equery(SQL, [GroupID]),
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


-spec windows(GroupID) -> Windows when
    GroupID :: group_id(),
    Windows :: [window_config()].

windows(GroupID) ->
    WindowSQL = "SELECT id, type, aggregation, interval, count "
                "FROM window_configurations WHERE group_id = $1;",
    {ok, _, Windows} = mpgsql:equery(WindowSQL, [GroupID]),
    Windows.


-spec format_group(GroupID, OwnerID, Priority, Tags) -> Group when
    GroupID :: group_id(),
    OwnerID :: owner_id(),
    Priority :: pos_integer(),
    Tags :: proplists:proplist(),
    Group :: proplists:proplist().

format_group(GroupID, OwnerID, Priority, Tags) ->
    RawWindows = windows(GroupID),
    Windows = lists:map(
        fun({WindowID, Type, Aggregation, Interval, Count}) ->
            [
                {id, WindowID},
                {type, binary_to_term(Type)},
                {aggregation, binary_to_term(Aggregation)},
                {interval, Interval},
                {count, Count}
            ]
        end,
        RawWindows
    ),
    [
        {id, GroupID},
        {owner_id, OwnerID},
        {priority, Priority},
        {tags, lists:map(fun list_to_tuple/1, Tags)},
        {windows, Windows}
    ].
