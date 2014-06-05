-module(oscilloscope_persistence).

-compile([{parse_transform, lager_transform}]).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    persist/2,
    vacuum/2,
    read/3
]).

-define(GUESS, 150).

-include_lib("oscilloscope_persistence/include/oscilloscope_persistence.hrl").
-include_lib("oscilloscope/include/oscilloscope_types.hrl").


-spec persist(Resolution, Points) -> {ok, Persisted} when
    Resolution :: oscilloscope_metadata_resolution:resolution(),
    Points :: [{timestamp(), number()}],
    Persisted :: [{timestamp(), number()}].

persist(Resolution, Points) ->
    Commutator = commutator(),
    ResolutionID = oscilloscope_metadata_resolution:id(Resolution),
    {ok, MinChunkSize} = application:get_env(
        oscilloscope_persistence,
        min_chunk_size
    ),
    {ok, MaxChunkSize} = application:get_env(
        oscilloscope_persistence,
        max_chunk_size
    ),
    Chunks = chunkify(Points, MinChunkSize, MaxChunkSize),
    lager:error(
        "Got chunks ~p for cache ~p, attempting to persist",
        [Chunks, ResolutionID]
    ),
    Persisted = lists:map(
        fun({Timestamp, Value, Size}) ->
            {ok, true} = commutator:put_item(
                Commutator,
                [ResolutionID, Timestamp, Value]
            ),
            ok = oscilloscope_metadata_resolution:insert_persist(
                Resolution,
                Timestamp,
                Size
            ),
            lager:debug(
                "Persist attempt successful for resolution ~p",
                [ResolutionID]
            ),
            {Timestamp, Size}
        end,
        Chunks
    ),
    {ok, Persisted}.


-spec vacuum(Resolution, Timestamps) -> {ok, Timestamps} when
    Resolution :: oscilloscope_metadata_resolution:resolution(),
    Timestamps :: [timestamp()].

vacuum(Resolution, Timestamps) ->
    Commutator = commutator(),
    ResolutionID = oscilloscope_metadata_resolution:id(Resolution),
    Vacuumed = lists:map(
        fun(T) ->
            {ok, true} = commutator:delete_item(Commutator, [ResolutionID, T]),
            ok = oscilloscope_metadata_resolution:delete_persist(Resolution, T),
            T
        end,
        Timestamps
    ),
    {ok, Vacuumed}.


-spec read(Resolution, From, Until) -> {ok, Read} when
    Resolution :: oscilloscope_metadata_resolution:resolution(),
    From :: timestamp(),
    Until :: timestamp(),
    Read :: read().

read(Resolution, From0, Until0) ->
    Commutator = commutator(),
    ID = oscilloscope_metadata_resolution:id(Resolution),
    Interval = oscilloscope_metadata_resolution:interval(Resolution),
    Persisted = oscilloscope_metadata_resolution:persisted(Resolution),
    {From1, Until1} = oscilloscope_util:adjust_query_range(
        From0,
        Until0,
        Interval
    ),
    Bounds = calculate_query_bounds(
        From1,
        Until1,
        Interval,
        Persisted
    ),
    Reply = case Bounds of
        not_found ->
            not_found;
        {ReadFrom, ReadUntil} ->
            {ok, Rows} = commutator:query(
                Commutator,
                [
                    {<<"id">>, equals, [ID]},
                    {<<"t">>, between, [ReadFrom, ReadUntil]}
                ]
            ),
            Read = lists:flatten(
                [?VALDECODE(proplists:get_value(<<"v">>, I)) || I <- Rows]
            ),
            {From2, Until2, TrimmedRead} = trim_read(
                From1,
                Until1,
                Interval,
                ReadFrom,
                Read
            ),
            {From2, Until2, TrimmedRead}
    end,
    {ok, Reply}.


commutator() ->
    {ok, Table} = application:get_env(oscilloscope_persistence, dynamo_table),
    {ok, Schema} = application:get_env(oscilloscope_persistence, dynamo_schema),
    {ok, Region} = application:get_env(oscilloscope_persistence, dynamo_region),
    {ok, AccessKey} = application:get_env(
        oscilloscope_persistence,
        dynamo_access_key
    ),
    {ok, SecretKey} = application:get_env(
        oscilloscope_persistence,
        dynamo_secret_key
    ),
    {ok, Commutator} = commutator:init(
        Table,
        Schema,
        Region,
        AccessKey,
        SecretKey
    ),
    Commutator.


-spec chunkify(Points, MinChunkSize, MaxChunkSize) -> Chunked when
    Points :: [{timestamp(), number()}],
    MinChunkSize :: pos_integer(),
    MaxChunkSize :: pos_integer(),
    Chunked :: [{timestamp(), binary(), integer()}].

chunkify(Points, MinChunkSize, MaxChunkSize) ->
    {Timestamps, Values} = lists:unzip(Points),
    chunkify(
        Timestamps,
        Values,
        [],
        MinChunkSize,
        MaxChunkSize,
        0,
        [],
        length(Values)
    ).

chunkify(Timestamps, Values, Excess, Min, Max, Count, Chunks, Guess) ->
    %% N.B.: This will OOM your BEAM if Min < ?VALENCODE([]).
    %% As of this comment, ?VALENCODE([]) == 11
    lager:error("Chunking ~p, ~p with guess ~p", [Values, Excess, Guess]),
    Chunk = ?VALENCODE(Values),
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
                Timestamps,
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
                        Timestamps,
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
            {_, Timestamps1} = lists:split(PointsChunked, Timestamps),
            Chunks1 = [{hd(Timestamps), Chunk, PointsChunked}|Chunks],
            %% Never try to split past the end of the list
            {Left, Right} = lists:split(min(Guess, length(Excess)), Excess),
            chunkify(
                Timestamps1,
                Left,
                Right,
                Min,
                Max,
                Count + PointsChunked,
                Chunks1,
                Guess
            )
    end.


-spec calculate_query_bounds(F, U, I, P) -> {F, U} | not_found when
    F :: timestamp(),
    U :: timestamp(),
    I :: interval(),
    P :: persisted().

%% TODO: TESTME
calculate_query_bounds(From, Until, Interval, Persisted) ->
    {InRange, _OutOfRange} = lists:foldr(
        fun({T, C}, {In, Out}) ->
            End = T + C * Interval,
            case T > Until orelse End < From of
                true -> {In, [T|Out]};
                false -> {[T|In], Out}
            end
        end,
        {[], []},
        Persisted
    ),
    case InRange of
        [] ->
            not_found;
        _Else ->
            {hd(InRange), lists:last(InRange)}
    end.


-spec trim_read(From, Until, Interval, ReadFrom, Read) -> {F, U, Points} when
    From :: timestamp(),
    Until :: timestamp(),
    Interval :: interval(),
    ReadFrom :: timestamp(),
    Read :: [value()],
    F :: timestamp(),
    U :: timestamp(),
    Points :: [value()].
%% TODO: TESTME
trim_read(From, Until, Interval, ReadFrom, Read) ->
    StartIndex = ((From - ReadFrom) div Interval),
    %% Until and From are both inclusive, so add one
    PointCount = ((Until - From) div Interval) + 1,
    %% Add one for 1-indexing
    Points = lists:sublist(Read, StartIndex + 1, PointCount),
    StartTime = ReadFrom + StartIndex * Interval,
    %% Subtract one for that nasty inclusive end again
    EndTime = StartTime + (length(Points) - 1) * Interval,
    {StartTime, EndTime, Points}.
