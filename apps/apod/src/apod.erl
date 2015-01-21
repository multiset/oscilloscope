-module(apod).
-on_load(init/0).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    new/5,
    to_list/1,
    read/3,
    update/2,
    update/3,
    truncate/2,
    earliest_time/1,
    latest_time/1,
    interval/1,
    size/1,
    chunkify/1,
    inflate/1,
    deflate/1
]).

-include_lib("osc/include/osc_types.hrl").

-opaque apod() :: term().

-export_type([apod/0]).

-define(
    NOTLOADED,
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]})
).

new(_Type, _Aggregation, _Interval, _Count, _Floor) ->
    ?NOTLOADED.


-spec to_list(Apod) -> Values when Apod :: apod(), Values :: [value()].

to_list(_Apod) ->
    ?NOTLOADED.


-spec read(Apod, From, Until) -> {From, Until, Values} when
    Apod :: apod(),
    From :: timestamp(),
    Until :: timestamp(),
    Values :: [value()].

read(_Apod, _From, _Until) ->
    ?NOTLOADED.


-spec update(Apod, Points) -> ok when
    Apod :: apod(),
    Points :: [{timestamp(), value()}].

update(Apod, Points) ->
    lists:foreach(fun({T, V}) -> ok = update(Apod, T, V) end, Points).


-spec update(Apod, Timestamp, Value) -> ok when
    Apod :: apod(),
    Timestamp :: timestamp(),
    Value :: value().

update(_Apod, _Timestamp, _Value) ->
    ?NOTLOADED.


-spec truncate(Apod, FloorTime) -> ok when
    Apod :: apod(),
    FloorTime :: timestamp().

truncate(_Apod, _FloorTime) ->
    ?NOTLOADED.


-spec earliest_time(Apod) -> Timestamp when
    Apod :: apod(),
    Timestamp :: timestamp().

earliest_time(_Apod) ->
    ?NOTLOADED.


-spec latest_time(Apod) -> Timestamp when
    Apod :: apod(),
    Timestamp :: timestamp().

latest_time(_Apod) ->
    ?NOTLOADED.


-spec interval(Apod) -> Interval when
    Apod :: apod(),
    Interval :: interval().

interval(_Apod) ->
    ?NOTLOADED.


-spec size(Apod) -> Size when
    Apod :: apod(),
    Size :: pos_integer().

size(_Apod) ->
    ?NOTLOADED.


-spec chunkify(Apod) -> Chunked when
    Apod :: apod(),
    Chunked :: [{timestamp(), binary(), integer()}].

chunkify(Apod) ->
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
    chunkify(Apod, MinPersistAge, MinChunkSize, MaxChunkSize).

chunkify(Apod, MinPersistAge, MinChunkSize, MaxChunkSize) ->
    %% This head exists largely for testing reasons.
    {From, _Until, Values} = read(Apod, 0, latest_time(Apod) - MinPersistAge),
    chunkify(
        From,
        interval(Apod),
        Values,
        [],
        MinChunkSize,
        MaxChunkSize,
        0,
        [],
        length(Values)
    ).

chunkify(T0, Interval, Values, Excess, Min, Max, Count, Chunks, Guess) ->
    %% We may want to write this in C at some point, but it's pretty easy to
    %% re-implement in Erlang for now.
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


-spec inflate(Binary) -> Value when
    Binary :: binary(),
    Value :: [value()].

inflate(<<0, Binary/binary>>) ->
    Bin = zlib:uncompress(Binary),
    inflate_int(Bin, []);
inflate(_Binary) ->
    {error, unknown_tag}.

inflate_int(<<>>, Acc) ->
    Acc;
inflate_int(Bin0, Acc) ->
    try <<V0:64/float, Bin1/binary>> = Bin0, {V0, Bin1} of
        {V, Bin2} -> inflate_int(Bin2, [V|Acc])
    catch error:{badmatch, _} ->
        %% Erlang just badmatches on Nan decodes, so catch them
        <<_:8/binary, Bin2/binary>> = Bin0,
        inflate_int(Bin2, [undefined|Acc])
    end.


-spec deflate(Values) -> Binary when
    Values :: [value()],
    Binary :: binary().

deflate(Values) ->
    deflate(Values, 0).

deflate(Values, 0) ->
    Bin = lists:foldl(
        fun(I, Acc) ->
            case I of
                undefined ->
                    %% This is a "significant" NaN as per IEEE 754
                    NaN = <<255, 240, 0, 0, 0, 0, 0, 1>>,
                    <<NaN/binary, Acc/binary>>;
                _ ->
                    <<I:64/float, Acc/binary>>
            end
        end,
        <<>>,
        Values
    ),
    Value = zlib:compress(Bin),
    <<0, Value/binary>>.


init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EBinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EBinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "apod"), 0).
