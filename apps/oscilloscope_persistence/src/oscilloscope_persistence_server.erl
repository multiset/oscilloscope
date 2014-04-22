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
    {reply, {ok, []}, State};
handle_call({read, CacheId, StartTime, EndTime}, _From, State) ->
    {reply, {ok, []}, State};
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
    chunkify(Timestamps, Values, [], ChunkMin, ChunkMax, 0, []).

chunkify(Timestamps, Values, Excess, Min, Max, Count, Chunks) ->
    %% N.B.: This will OOM your BEAM if Min < ?VALENCODE([]).
    %% As of this comment, ?VALENCODE([]) == 11
    Chunk = ?VALENCODE(Values),
    case byte_size(Chunk) of
        Size when Size > Max ->
            {Left, Right} = bisect(Values),
            chunkify(Timestamps, Left, Right ++ Excess, Min, Max, Count, Chunks);
        Size when Size < Min ->
            case Excess of
                [] ->
                    %% No more points to try - bail out with what we've chunked.
                    lists:reverse(Chunks);
                _ ->
                    {Left, Right} = bisect(Excess),
                    chunkify(
                        Timestamps,
                        Values ++ Left,
                        Right,
                        Min,
                        Max,
                        Count,
                        Chunks
                    )
            end;
        Size ->
            PointsChunked = length(Values),
            {_, Timestamps1} = lists:split(PointsChunked, Timestamps),
            Chunks1 = [{hd(Timestamps), Chunk, PointsChunked}|Chunks],
            chunkify(
                Timestamps1,
                Excess,
                [],
                Min,
                Max,
                Count + PointsChunked,
                Chunks1
            )
    end.

bisect(List) ->
    lists:split(round(length(List) / 2), List).


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
        50,
        75
    ),
    Decoded1 = lists:map(fun({I, V, _}) -> {I, ?VALDECODE(V)} end, Chunked1),
    ?assertEqual([
        {0, [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]},
        {110, [11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0]}
    ], Decoded1).

-endif.
