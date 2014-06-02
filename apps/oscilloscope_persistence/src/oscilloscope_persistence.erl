-module(oscilloscope_persistence).

-export([
    persist/2,
    vacuum/2,
    read/3
]).

-define(VALENCODE(V), zlib:compress(term_to_binary(V))).
-define(VALDECODE(V), binary_to_term(zlib:uncompress(V))).
-define(GUESS, 150).

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
    lager:debug(
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
    {From, Until, Points} = case calculate_starttime(From0, Persisted) of
        not_found ->
            {From0, From0, []};
        From1 ->
            Until1 = calculate_endtime(Until0, Persisted),
            {ok, Rows} = commutator:query(
                Commutator,
                [
                    {<<"id">>, equals, [ID]},
                    {<<"t">>, between, [From1, Until1]}
                ]
            ),
            Read = lists:flatten(
                [?VALDECODE(proplists:get_value(<<"v">>, I)) || I <- Rows]
            ),
            {From1, From1 + (Interval * length(Read)), Read}
    end,
    {ok, {From, Until, Points}}.

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

calculate_starttime(T, Ts) ->
    case lists:partition(fun({T1, _C}) -> T1 =< T end, Ts) of
        {[], []} ->
            not_found;
        {[], [{Time, _Count}|_]} ->
            Time;
        {Earlier, _} ->
            {Time, _Count} = lists:last(Earlier),
            Time
    end.

calculate_endtime(T, Ts) ->
    case lists:partition(fun({T1, _C}) -> T1 >= T end, Ts) of
        {[], _} -> T;
        {[{Time, _Count}|_], _} ->
            Time
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

chunkify_test() ->
    %% No chunking
    Input = [{0, 1.0}, {10, 2.0}, {20, 3.0}],
    ?assertEqual(
        [],
        chunkify(
            Input,
            1000000,
            1000000
        )
    ),
    %% Chunking each value
    Chunked = chunkify(
        Input,
        11,
        1000000
    ),
    Decoded = lists:map(fun({I, V, _}) -> {I, ?VALDECODE(V)} end, Chunked),
    ?assertEqual([{0, [1.0, 2.0, 3.0]}], Decoded),
    %% Chunking multiple values together
    Input1 = [{I * 10, float(I)} || I <- lists:seq(0, 21)],
    Chunked1 = chunkify(
        Input1,
        30,
        65
    ),
    Decoded1 = lists:map(fun({I, V, _}) -> {I, ?VALDECODE(V)} end, Chunked1),
    ?assertEqual([
        {0, [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0]},
        {120, [12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0]}
    ], Decoded1).

calculate_starttime_test() ->
    ?assertEqual(2, calculate_starttime(3, [{2, 2}, {4, 2}, {6, 2}, {8, 2}])),
    ?assertEqual(2, calculate_starttime(1, [{2, 2}, {4, 2}, {6, 2}, {8, 2}])).

calculate_endtime_test() ->
    ?assertEqual(4, calculate_endtime(3, [{2, 2}, {4, 2}, {6, 2}, {8, 2}])),
    ?assertEqual(9, calculate_endtime(9, [{2, 2}, {4, 2}, {6, 2}, {8, 2}])).

-endif.
