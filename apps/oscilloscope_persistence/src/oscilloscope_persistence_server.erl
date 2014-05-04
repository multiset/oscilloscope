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

handle_call({persist, CacheId, Points}, _From, State) ->
    #st{
        commutator = Commutator,
        min_chunk_size = MinChunkSize,
        max_chunk_size = MaxChunkSize
    } = State,
    Chunks = chunkify(Points, MinChunkSize, MaxChunkSize),
    Persisted = lists:map(
        fun({Timestamp, Value, Size}) ->
            {ok, true} = commutator:put_item(
                Commutator,
                [CacheId, Timestamp, Value]
            ),
            {Timestamp, Size}
        end,
        Chunks
    ),
    {reply, {ok, Persisted}, State};
handle_call({vacuum, CacheId, Timestamps}, _From, State) ->
    #st{
        commutator = Commutator
    } = State,
    Persists = lists:map(
        fun(T) ->
            {T, commutator:delete_item(Commutator, [CacheId, T])}
        end,
        Timestamps
    ),
    Successes = lists:filtermap(
        fun({T, Res}) ->
            Res =:= {ok, true} andalso T
        end,
        Persists
    ),
    {reply, {ok, Successes}, State};
handle_call({read, CacheId, StartTime, EndTime}, _From, State) ->
    #st{
        commutator = Commutator
    } = State,
    {ok, Rows} = commutator:query(
        Commutator,
        [{<<"id">>, equals, [CacheId]}, {<<"t">>, between, [StartTime, EndTime]}]
    ),
    Points = lists:flatten(
        [?VALDECODE(proplists:get_value(<<"v">>, I)) || I <- Rows]
    ),
    {reply, {ok, Points}, State};
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
        Size ->
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

-endif.
