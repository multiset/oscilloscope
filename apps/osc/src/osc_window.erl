-module(osc_window).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    new/1,
    read/3,
    update/2,
    refresh/2,
    trim/1,
    chunkifyability/1,
    chunkify/1,
    inflate/1,
    deflate/1,
    now/1,
    earliest_time/1
]).

-include_lib("osc/include/osc_types.hrl").

-record(window, {
    interval :: interval(),
    count :: count(),
    latest_persisted_time :: timestamp() | undefined,
    aggregation :: fun((wrapped_value()) -> value()),
    t :: timestamp() | undefined,
    points :: array()
}).

-opaque window() :: #window{}.
-export_type([window/0]).


-spec new(Meta) -> Window when
    Meta :: osc_meta_window:windowmeta(),
    Window :: window().

new(Meta) ->
    AggregationAtom = osc_meta_window:aggregation(Meta),
    AggregationFun = fun(Vals) ->
        erlang:apply(osc_aggregations, AggregationAtom, [Vals])
    end,
    #window{
        interval = osc_meta_window:interval(Meta),
        count = osc_meta_window:count(Meta),
        latest_persisted_time = osc_meta_window:latest_persisted_time(Meta),
        aggregation = AggregationFun,
        t = undefined,
        points = array:new({default, null})
    }.


-spec read(From, Until, Window) -> {ok, Read} | {error, Error} when
    From :: timestamp(),
    Until :: timestamp(),
    Window :: window(),
    Read :: {timestamp(), timestamp(), [value()]} | no_data,
    Error :: atom().

read(From, Until, Window) ->
    #window{
        interval=Interval,
        aggregation=Aggregation,
        t=T,
        points=Points
    } = Window,
    Read = read_int(From, Until, Interval, Aggregation, T, Points),
    {ok, Read}.


-spec update(Points, Window) -> Window when
    Points :: [{timestamp(), value()}],
    Window :: window().

update(IncomingPoints, Window) ->
    #window{
        interval=Interval,
        latest_persisted_time=LatestPersisted,
        t=T0,
        points=Points0
    } = Window,
    {T1, Points1} = append(
        IncomingPoints,
        T0,
        Points0,
        Interval,
        LatestPersisted
    ),
    trim(Window#window{t=T1, points=Points1}).


-spec refresh(Meta, Window) -> Window when
    Meta :: osc_meta_window:windowmeta(),
    Window :: window().

refresh(Meta, Window) ->
    Window#window{
        interval = osc_meta_window:interval(Meta),
        count = osc_meta_window:count(Meta),
        latest_persisted_time = osc_meta_window:latest_persisted_time(Meta)
    }.


-spec trim(Window) -> Window when
    Window :: window().

trim(Window) ->
    #window{
        interval=Interval,
        count=Count,
        latest_persisted_time=LatestPersisted,
        t=T0,
        points=Points0
    } = Window,
    {T1, Points1} = maybe_trim(T0, Points0, Interval, Count, LatestPersisted),
    Window#window{t=T1, points=Points1}.


-spec chunkifyability(Window) -> Chunkifyability when
    Window :: window(),
    Chunkifyability :: float().

chunkifyability(_Window) ->
    random:uniform().


-spec chunkify(Window) -> Chunks when
    Window :: window(),
    Chunks :: [{Timestamp, Chunk, Count}],
    Timestamp :: timestamp(),
    Chunk :: binary(),
    Count :: pos_integer().

chunkify(Window) ->
    #window{
        interval = Interval,
        aggregation = Aggregation,
        t = T,
        points = Points
    } = Window,
    {ok, MinChunkSize} = application:get_env(
        osc_persistence,
        min_chunk_size
    ),
    {ok, MaxChunkSize} = application:get_env(
        osc_persistence,
        max_chunk_size
    ),
    {ok, MinPersistAge} = application:get_env(
        osc_persistence,
        min_persist_age
    ),
    %% TODO: test this
    LatestTime = T + (array:size(Points) - 1) * Interval,
    LatestPersist = LatestTime - MinPersistAge,
    LatestIndex = (LatestPersist - T) / Interval,
    {PointsToChunk0, _} = divide_array(Points, LatestIndex),
    PointsToChunk1 = lists:map(Aggregation, PointsToChunk0),
    chunkify(T, Interval, PointsToChunk1, MinChunkSize, MaxChunkSize).

-spec inflate(Binary) -> Value when
    Binary :: binary(),
    Value :: [value()].

inflate(Binary) -> binary_to_term(zlib:uncompress(Binary)).


-spec deflate(Values) -> Binary when
    Values :: [value()],
    Binary :: binary().

deflate(Values) -> zlib:compress(term_to_binary(Values)).


-spec now(Window) -> Timestamp when
    Window :: window(),
    Timestamp :: timestamp() | undefined.

now(Window) ->
    #window{
        t=T,
        points=Points,
        interval=Interval,
        latest_persisted_time=LatestPersistedTime
    }=Window,
    case T of
        undefined ->
            LatestPersistedTime;
        T ->
            T + (Interval * array:size(Points))
    end.


-spec earliest_time(Window) -> Timestamp when
    Window :: window(),
    Timestamp :: timestamp() | undefined.

earliest_time(Window) ->
    Window#window.t.

-spec read_int(From, Until, Interval, Aggregation, T, Points) -> Read when
    From :: timestamp(),
    Until :: timestamp(),
    Interval :: interval(),
    Aggregation :: fun((wrapped_value()) -> value()),
    T :: timestamp(),
    Points :: array:array(wrapped_value()),
    Read :: {timestamp(), timestamp(), [value()]} | no_data.

read_int(From0, Until0, Interval, Aggregation, T, Points) ->
    {From1, Until1} = osc_util:adjust_query_range(From0, Until0, Interval),
    case T =< Until1 of
        false ->
            no_data;
        true ->
            %% At least some of the query is in the cache
            StartIndex = erlang:max(0, (From1 - T) div Interval),
            EndIndex = erlang:min(
                array:size(Points) - 1,
                (Until1 - T) div Interval
            ),
            From2 = T + StartIndex * Interval,
            Until2 = T + EndIndex * Interval,
            Values = range_from_array(
                StartIndex,
                EndIndex,
                Aggregation,
                Points
            ),
            {From2, Until2, Values}
    end.


-spec append(IncomingPoints, T, Points, Interval, FloorTime) -> {T, Points} when
    IncomingPoints :: [{timestamp(), value()}],
    T :: timestamp(),
    Points :: array(),
    Interval :: interval(),
    FloorTime :: timestamp() | undefined.

append([], T, Points, _Interval, _LatestPersisted) ->
    {T, Points};
append([{Timestamp0, Value}|Ps], T0, Points0, Interval, LatestPersisted) ->
    %% Timestamps are always floored to fit intervals exactly
    Timestamp1 = Timestamp0 - (Timestamp0 rem Interval),
    %% Any point that's newer than the last persist time is acceptable, but
    %% we'll never try to overwrite a previously-persisted index.
    BeyondLatestPersisted = case LatestPersisted of
        undefined -> true;
        _ -> Timestamp1 >= (LatestPersisted + Interval)
    end,
    {T1, Points1} = case {BeyondLatestPersisted, T0} of
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
    append(Ps, T1, Points1, Interval, LatestPersisted).


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


-spec maybe_trim(Timestamp, Points, I, C, LP) -> {Timestamp, Points} when
    Timestamp :: timestamp(),
    Points :: array:array(wrapped_value()),
    I :: interval(),
    C :: pos_integer(),
    LP :: timestamp() | undefined.

maybe_trim(undefined, Points, _Interval, _Count, _LastPersisted) ->
    {undefined, Points};
maybe_trim(T0, Points0, Interval, Count, LastPersisted) ->
    %% The newest point currently stored in memory
    LatestTime = T0 + (array:size(Points0) - 1) * Interval,
    %% The earliest point that should be stored given the window configuration
    EarliestTime = LatestTime - Interval * (Count - 1), % Inclusive
    T1 = case LastPersisted of
        undefined -> EarliestTime;
        _ -> max(LastPersisted + Interval, EarliestTime)
    end,
    SplitIndex = (T1 - T0) / Interval,
    case SplitIndex > 0 of
        false ->
            {T0, Points0};
        true ->
            {_, Points1} = divide_array(Points0, SplitIndex),
            {T1, array:from_list(Points1, null)}
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


-spec chunkify(T, Interval, Points, MinChunkSize, MaxChunkSize) -> Chunked when
    T :: timestamp(),
    Interval :: interval(),
    Points :: [number()],
    MinChunkSize :: pos_integer(),
    MaxChunkSize :: pos_integer(),
    Chunked :: [{timestamp(), binary(), integer()}].

chunkify(T, Interval, Points, MinChunkSize, MaxChunkSize) ->
    chunkify(
        T,
        Interval,
        Points,
        [],
        MinChunkSize,
        MaxChunkSize,
        0,
        [],
        length(Points)
    ).

chunkify(T0, Interval, Values, Excess, Min, Max, Count, Chunks, Guess) ->
    %% N.B.: This will OOM your BEAM if Min < ?VALENCODE([]).
    %% As of this comment, ?VALENCODE([]) == 11
    lager:debug("Chunking ~p, ~p with guess ~p", [Values, Excess, Guess]),
    Chunk = deflate(Values),
    PointsChunked = length(Values),
    case byte_size(Chunk) of
        Size when Size > Max ->
            %% The chunk was too big. Generate a new guess based on the
            %% average compressed point size, shrink Values as appropriate, and
            %% try again.
            BytesPerPoint = Size / PointsChunked,
            ChunkGuess = Max / BytesPerPoint,
            %% Always decrement the guess by at least one
            NewGuess = min(
                Guess - 1,
                round(Guess - (Guess - ChunkGuess) / 2)
            ),
            %% Never try to split past the end of the list
            {Left, Right} = lists:split(min(NewGuess, PointsChunked), Values),
            chunkify(
                T0,
                Interval,
                Left,
                Right ++ Excess,
                Min,
                Max,
                Count,
                Chunks,
                NewGuess
            );
        Size when Size < Min ->
            case Excess of
                [] ->
                    %% No more points to try - bail out with what we've chunked.
                    lists:reverse(Chunks);
                _ ->
                    %% The chunk was too small. Generate a new guess based on
                    %% new data, pull points from the front of Excess, and try
                    %% again.
                    BytesPerPoint = Size / PointsChunked,
                    ChunkGuess = Min / BytesPerPoint,
                    %% Always increment the Guess by at least one
                    NewGuess = max(
                        Guess + 1,
                        round(Guess + (ChunkGuess - Guess) / 2)
                    ),
                    %% Never try to split past the end of the list
                    {Left, Right} = lists:split(
                        min((NewGuess + 1) - PointsChunked, length(Excess)),
                        Excess
                    ),
                    chunkify(
                        T0,
                        Interval,
                        Values ++ Left,
                        Right,
                        Min,
                        Max,
                        Count,
                        Chunks,
                        NewGuess
                    )
            end;
        _Size ->
            T1 = T0 + (Interval * PointsChunked),
            Chunks1 = [{T0, Chunk, PointsChunked}|Chunks],
            %% Never try to split past the end of the list
            {Left, Right} = lists:split(min(Guess, length(Excess)), Excess),
            chunkify(
                T1,
                Interval,
                Left,
                Right,
                Min,
                Max,
                Count + PointsChunked,
                Chunks1,
                Guess
            )
    end.
