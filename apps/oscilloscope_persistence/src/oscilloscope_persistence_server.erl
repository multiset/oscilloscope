-module(oscilloscope_persistence_server).

-behaviour(gen_server).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("oscilloscope/include/oscilloscope_types.hrl").

-define(VALENCODE(V), zlib:compress(term_to_binary(V))).
-define(VALDECODE(V), binary_to_term(zlib:uncompress(V))).
-define(GUESS, 150).

-record(st, {
    commutator,
    min_chunk_size,
    max_chunk_size,
    min_persist_age
}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init({Commutator, MinChunkSize, MaxChunkSize}) ->
    {ok, #st{
        commutator=Commutator,
        min_chunk_size=MinChunkSize,
        max_chunk_size=MaxChunkSize
    }}.

handle_call({persist, Resolution, Points}, _From, State) ->
    #st{
        commutator = Commutator,
        min_chunk_size = MinChunkSize,
        max_chunk_size = MaxChunkSize
    } = State,
    ResolutionID = oscilloscope_metadata_resolution:id(Resolution),
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
    {reply, {ok, Persisted}, State};
handle_call({vacuum, Resolution, Timestamps}, _From, State) ->
    #st{
        commutator = Commutator
    } = State,
    ResolutionID = oscilloscope_metadata_resolution:id(Resolution),
    Vacuums = lists:map(
        fun(T) ->
            {ok, true} = commutator:delete_item(Commutator, [ResolutionID, T]),
            ok = oscilloscope_metadata_resolution:delete_persist(Resolution, T),
            T
        end,
        Timestamps
    ),
    {reply, {ok, Vacuums}, State};
handle_call({read, Resolution, From0, Until0}, _From, State) ->
    #st{
        commutator = Commutator
    } = State,
    {ID, Interval, _Count, Persisted} = Resolution,
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
    {reply, {ok, {From, Until, Resolution, Points}}, State};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, error, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec chunkify([{timestamp(), number()}], integer(), integer()) ->
  [{timestamp(), binary(), integer()}].
chunkify(Points, ChunkMin, ChunkMax) ->
    {Timestamps, Values} = lists:unzip(Points),
    chunkify(Timestamps, Values, [], ChunkMin, ChunkMax, 0, [], length(Values)).

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

