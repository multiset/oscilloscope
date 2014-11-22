-module(osc_cache).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    new/2,
    refresh/1,
    update/2,
    read/3,
    cached/2,
    fold/3
]).

-export_type([cache/0, resolution/0]).

-include_lib("osc/include/osc_types.hrl").

-record(cache, {
    metric :: metric(),
    meta :: osc_meta:meta(),
    aggregation :: fun((wrapped_value()) -> value()),
    resolutions :: [resolution()]
}).

-record(resolution, {
    meta :: osc_meta_resolution:resolution(),
    t :: timestamp(), %% Earliest point in array
    points :: array() %% Array of points
}).

-opaque cache() :: #cache{}.
-opaque resolution() :: #resolution{}.


-spec new(Metric, Meta) -> Cache when
    Metric :: metric(),
    Meta :: osc_meta:meta(),
    Cache :: cache().

new(Metric, Meta) ->
    AggregationAtom = osc_meta_metric:aggregation(Meta),
    AggregationFun = fun(Vals) ->
        erlang:apply(osc_aggregations, AggregationAtom, [Vals])
    end,
    Resolutions = lists:map(
        fun(R) ->
            #resolution{
                meta=R,
                t=undefined,
                points=array:new({default, null})
            }
        end,
        osc_meta_metric:resolutions(Meta)
    ),
    #cache{
        metric=Metric,
        meta=Meta,
        aggregation=AggregationFun,
        resolutions=Resolutions
    }.


-spec refresh(Cache) -> Cache when
    Cache :: cache().

refresh(Cache) ->
    #cache{
        metric=Metric,
        meta=Meta0,
        aggregation=AF0,
        resolutions=Resolutions0
    } = Cache,
    {ok, Meta1} = osc_meta_metric:lookup(Metric),
    Aggregation0 = osc_meta_metric:aggregation(Meta0),
    Aggregation1 = osc_meta_metric:aggregation(Meta1),
    AF1 = case Aggregation0 == Aggregation1 of
        true ->
            AF0;
        false ->
            fun(Vals) ->
                erlang:apply(osc_aggregations, Aggregation1, [Vals])
            end
    end,
    NewResolutions = osc_meta_metric:resolutions(Meta1),
    %% TODO: Handle addition or removal of resolutions
    Resolutions1 = lists:map(
        fun(R) -> refresh_resolution(R, NewResolutions) end,
        Resolutions0
    ),
    Cache#cache{aggregation=AF1, resolutions=Resolutions1}.


-spec update(Points, Cache) -> Cache when
    Points :: [{timestamp(), value()}],
    Cache :: cache().

update(Points, Cache) ->
    #cache{resolutions=Resolutions0} = Cache,
    Resolutions1 = lists:map(
        fun(Resolution) -> update_int(Points, Resolution) end,
        Resolutions0
    ),
    Cache#cache{resolutions=Resolutions1}.


-spec read(From, Until, Cache) -> {ok, Read} | {error, Error} when
    From :: timestamp(),
    Until :: timestamp(),
    Cache :: cache(),
    Read :: cache_read(),
    Error :: atom().

read(From, Until, _Cache) when From > Until ->
    {error, temporal_inversion};
read(From0, Until0, Cache) ->
    #cache{meta=Meta, resolutions=Resolutions, aggregation=Aggregation} = Cache,
    #resolution{meta=Resolution, t=T, points=Points} = select_resolution(
        From0,
        Resolutions
    ),
    Interval = osc_meta_resolution:interval(Resolution),
    Read = read_int(
        From0,
        Until0,
        Interval,
        Aggregation,
        T,
        Points
    ),
    {ok, {Meta, Resolution, Read}}.


-spec cached(Cache, Resolution) -> {Points, Meta} when
    Cache :: cache(),
    Resolution :: resolution(),
    Points :: [{timestamp(), number()}],
    Meta :: osc_meta_resolution:resolution().

cached(Cache, Resolution) ->
    #cache{aggregation=AF} = Cache,
    #resolution{meta=Meta, t=T, points=PointsArray} = Resolution,
    Interval = osc_meta_resolution:interval(Meta),
    Points = array:foldr(
        fun(Idx, Values, Acc) -> [{T + (Idx * Interval), AF(Values)}|Acc] end,
        [],
        PointsArray
    ),
    {Points, Meta}.


-spec fold(Fun, Acc, Cache) -> Acc when
    Fun :: fun((Cache, resolution(), Acc) -> Acc),
    Acc :: any(),
    Cache :: cache().

fold(Fun, Acc, Cache) ->
    #cache{resolutions=Resolutions} = Cache,
    lists:foldl(fun(R, A) -> Fun(Cache, R, A) end, Acc, Resolutions).


-spec refresh_resolution(Resolution, Metas) -> Resolution when
    Resolution :: resolution(),
    Metas :: [osc_meta_resolution:resolution()].

refresh_resolution(Resolution, Metas) ->
    %% TODO: This only handles persistence updates
    #resolution{meta=Meta0, t=T0, points=Points0} = Resolution,
    ID = osc_meta_resolution:id(Meta0),
    Interval = osc_meta_resolution:interval(Meta0),
    [Meta1] = lists:filter(
        fun(R) -> osc_meta_resolution:id(R) == ID end,
        Metas
    ),
    LatestPersist = osc_meta_resolution:latest_persist_time(Meta1),
    {T1, Points1} = case LatestPersist of
        undefined ->
            {T0, Points0};
        T0 ->
            {T0, Points0};
        Else ->
            %% TODO: probably an off-by-one here
            Index = (Else - T0) div Interval,
            {_, PointsList} = divide_array(Points0, Index),
            {Else + Interval, array:from_list(PointsList, null)}
    end,
    #resolution{meta=Meta1, t=T1, points=Points1}.


-spec update_int(Points, Resolution) -> Resolution when
    Points :: [{timestamp(), value()}],
    Resolution :: resolution().

update_int(Points, Resolution) ->
    #resolution{meta=Meta, t=T0, points=Points0} = Resolution,
    Interval = osc_meta_resolution:interval(Meta),
    Count = osc_meta_resolution:count(Meta),
    Persisted = osc_meta_resolution:persisted(Meta),
    {T1, Points1} = append(Points, T0, Points0, Interval, Persisted),
    {T2, Points2} = maybe_trim(T1, Points1, Interval, Count),
    Resolution#resolution{t=T2, points=Points2}.

-spec append(ToAppend, T, Points, Interval, Persisted) -> {T, Points} when
    ToAppend :: [{timestamp(), value()}],
    T :: timestamp(),
    Points :: array(),
    Interval :: interval(),
    Persisted :: persisted().

append([], T, Points, _Interval, _Persisted) ->
    {T, Points};
append([{Timestamp0, Value}|Ps], T0, Points0, Interval, Persisted) ->
    %% Timestamps are always floored to fit intervals exactly
    Timestamp1 = Timestamp0 - (Timestamp0 rem Interval),
    %% Any point that's newer than the last persist time is acceptable, but
    %% we'll never try to overwrite a previously-persisted index.
    LastPersist = last_persisted_time(Persisted, Interval),
    {T1, Points1} = case {Timestamp1 > LastPersist, T0} of
        {false, _} ->
            {T0, Points0};
        {true, undefined} ->
            {Timestamp1, append_point(0, Value, Points0)};
        {true, T0} when T0 =< Timestamp1 ->
            Index = (Timestamp1 - T0) div Interval,
            {T0, append_point(Index, Value, Points0)};
        {true, _} ->
            %% Need to slide the window backwards - this is a legal
            %% point that's at a negative index in the current array.
            IndexesToAdd = (T0 - Timestamp1) div Interval,
            {Timestamp1, prepend_point(IndexesToAdd, Value, Points0)}
    end,
    append(Ps, T1, Points1, Interval, Persisted).


-spec maybe_trim(Timestamp, Points, Interval, Count) -> {Timestamp, Points} when
    Timestamp :: timestamp(),
    Points :: array:array(wrapped_value()),
    Interval :: interval(),
    Count :: pos_integer().

maybe_trim(undefined, Points, _Interval, _Count) ->
    {undefined, Points};
maybe_trim(T, Points0, Interval, Count) ->
    LatestTime = T + (array:size(Points0) - 1) * Interval,
    EarliestTime = LatestTime - Interval * (Count - 1), % Inclusive
    SplitIndex = (EarliestTime - T) / Interval,
    case SplitIndex > 0 of
        false ->
            {T, Points0};
        true ->
            {_, Points1} = divide_array(Points0, SplitIndex),
            {EarliestTime, array:from_list(Points1, null)}
    end.


-spec prepend_point(Index, Value, Points) -> Points when
    Index :: array:array_indx(),
    Value :: value(),
    Points :: array:array(wrapped_value()).

prepend_point(Index, Value, Points) ->
    ListPoints = array:to_list(Points),
    Prepend = lists:duplicate(Index, null),
    Points1 = array:from_list(Prepend ++ ListPoints, null),
    append_point(0, Value, Points1).


-spec append_point(Index, Value, Points) -> Points when
    Index :: array:array_indx(),
    Value :: value(),
    Points :: array:array(wrapped_value()).

append_point(Index, Value, Points) when Index >= 0 ->
    case array:get(Index, Points) of
        null ->
            array:set(Index, [Value], Points);
        Vs when is_list(Vs) ->
            array:set(Index, [Value|Vs], Points)
    end.


-spec divide_array(Array, Index) -> Divided when
    Array :: array:array(wrapped_value()),
    Index :: array:array_indx(),
    Divided :: {[wrapped_value()], [wrapped_value()]}.

divide_array(Arr, DivIdx) ->
    array:foldr(
        fun(Idx, Value, {L, R}) ->
            case Idx < DivIdx of
                true -> {[Value|L], R};
                false -> {L, [Value|R]}
            end
        end,
        {[], []},
        Arr
    ).


-spec select_resolution(From, Resolutions) -> Resolution when
    From :: timestamp(),
    Resolutions :: [resolution()],
    Resolution :: resolution().

select_resolution(From, Resolutions) ->
    [R|Rs] = lists:sort(
        fun(#resolution{meta=MetaA}, #resolution{meta=MetaB}) ->
            IntervalA = osc_meta_resolution:interval(MetaA),
            IntervalB = osc_meta_resolution:interval(MetaB),
            IntervalA >= IntervalB
        end,
        Resolutions
    ),
    lists:foldl(
        fun(Resolution, Selected) ->
            case earliest_timestamp(Resolution) =< From of
                true -> Resolution;
                false -> Selected
            end
        end,
        R,
        Rs
    ).


-spec earliest_timestamp(Resolution) -> Timestamp when
    Resolution :: resolution(),
    Timestamp :: timestamp().

earliest_timestamp(Resolution) ->
    #resolution{meta=Meta, t=T} = Resolution,
    case osc_meta_resolution:earliest_persist_time(Meta) of
        undefined -> T;
        Timestamp -> Timestamp
    end.


-spec read_int(From, Until, Interval, Aggregation, T, Points) -> Read when
    From :: timestamp(),
    Until :: timestamp(),
    Interval :: interval(),
    Aggregation :: fun((wrapped_value()) -> value()),
    T :: timestamp(),
    Points :: array:array(wrapped_value()),
    Read :: read().

read_int(From0, Until0, Interval, Aggregation, T, Points) ->
    {From1, Until1} = osc_util:adjust_query_range(
        From0,
        Until0,
        Interval
    ),
    case T =< Until1 of
        false ->
            not_found;
        true ->
            %% At least some of the query is in the cache
            StartIndex = erlang:max(0, (From1 - T) div Interval),
            EndIndex = erlang:min(
                array:size(Points) - 1,
                (Until1 - T) div Interval
            ),
            From2 = T + StartIndex * Interval,
            Until2 = T + EndIndex * Interval,
            Read = range_from_array(
                StartIndex,
                EndIndex,
                Aggregation,
                Points
            ),
            {From2, Until2, Read}
    end.


-spec range_from_array(Start, End, Aggregation, Points) -> Acc when
    Start :: array:array_indx(),
    End :: array:array_indx(),
    Aggregation :: fun((wrapped_value()) -> value()),
    Acc :: [value()],
    Points :: array:array(wrapped_value()).

range_from_array(Start, End, AF, Points) ->
    lists:reverse(array:foldl(
        fun(I, Vs, Acc) when I >= Start andalso I =< End ->
               [AF(Vs)|Acc];
           (_I, _Vs, Acc) ->
               Acc
        end,
        [],
        Points
    )).


-spec last_persisted_time(Persisted, Interval) -> Timestamp when
    Persisted :: persisted(),
    Interval :: interval(),
    Timestamp :: integer(). %% Not timestamp() because it can be negative!

last_persisted_time(Persisted, Interval) ->
    case Persisted of
        [] ->
            -1 * Interval;
        Persisted ->
            {LastPersistTime, LastPersistCount} = lists:last(Persisted),
            LastPersistTime + Interval * LastPersistCount - 1
    end.
