-module(osc_meta_window_configuration).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    create/4,
    lookup/1
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
    {ok, _, {GroupID}} = osc_sql:adhoc(GroupSQL, [OwnerID, Priority, Tags]),
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
            {ok, _, _} = osc_sql:adhoc(WindowSQL, Args)
        end,
        WindowConfigs
    ),
    {ok, GroupID}.

-spec lookup(Metric) -> {ok, Windows} when
    Metric :: metric(),
    Windows :: [window_config()].

lookup({OwnerID, Props}) ->
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

