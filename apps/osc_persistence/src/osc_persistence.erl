-module(osc_persistence).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    persist/2,
    read/3,
    persist_int/2,
    read_int/3
]).

-include_lib("osc/include/osc_types.hrl").

-spec persist(WindowMeta, Window) -> ok when
    WindowMeta :: osc_meta_window:windowmeta(),
    Window :: apod:apod().

persist(WindowMeta, Window) ->
    mstat:increment_counter([osc_persistence, persist_attempts]),
    {ok, Timeout} = application:get_env(osc_persistence, request_timeout),
    poolboy:transaction(
        osc_persistence_pool,
        fun(Worker) ->
            %% TODO: don't use an infinite timeout. The current implementation
            %% is ~obligated to in order to maintain state consistency.
            %% Rewriting the persistence app as a pull-based system (instead of
            %% a push-based one) would prevent this from becoming an issue.
            gen_server:call(Worker, {persist, WindowMeta, Window}, infinity)
        end,
        Timeout
    ).

-spec read(WindowMeta, From, Until) -> {ok, Read | no_data} when
    WindowMeta :: osc_meta_window:windowmeta(),
    From :: timestamp(),
    Until :: timestamp(),
    Read :: {timestamp(), timestamp(), [value()]}.

read(WindowMeta, From, Until) ->
    {ok, Timeout} = application:get_env(osc_persistence, request_timeout),
    poolboy:transaction(
        osc_persistence_pool,
        fun(Worker) ->
            gen_server:call(Worker, {read, WindowMeta, From, Until})
        end,
        Timeout
    ).


-spec persist_int(WindowMeta, Window) -> {ok, PersistCount} when
    WindowMeta :: osc_meta_window:windowmeta(),
    Window :: apod:apod(),
    PersistCount :: non_neg_integer().

persist_int(WindowMeta, Window) ->
    Chunks = apod:chunkify(Window),
    case Chunks of
        [] ->
            mstat:increment_counter([osc_persistence, no_chunks_found]),
            ok;
        _ ->
            Commutator = osc_persistence_util:commutator(),
            WindowID = osc_meta_window:id(WindowMeta),
            lager:debug(
                "Got chunks ~p for window ~p, attempting to persist",
                [Chunks, WindowID]
            ),
            lists:foreach(
                fun({Timestamp, Value, Size}) ->
                    {ok, true} = commutator:put_item(
                        Commutator,
                        [WindowID, Timestamp, Value]
                    ),
                    InsertPersist = fun IP() ->
                        try
                            ok = osc_meta_window:insert_persist(
                                WindowMeta,
                                Timestamp,
                                Size
                            )
                        catch
                            error:{badmatch, {error, unique_violation}} ->
                                %% This was previously successfully inserted -
                                %% probably on a query that timed-out - so
                                %% return
                                ok;
                            error:{badmatch, B} ->
                                lager:warning(
                                    "badmatch in persist insert: ~p",
                                    [B]
                                ),
                                timer:sleep(trunc(1000 * random:uniform())),
                                IP()
                        end
                    end,
                    InsertPersist(),
                    mstat:increment_counter(
                        [osc_persistence, persisted_chunks]
                    ),
                    mstat:increment_counter(
                        [osc_persistence, persisted_points],
                        Size
                    ),
                    mstat:update_histogram([osc_persistence, chunk_size], Size),
                    lager:debug(
                        "Persist attempt successful for window ~p",
                        [WindowID]
                    ),
                    {Timestamp, Size}
                end,
                Chunks
            )
    end,
    {ok, length(Chunks)}.


-spec read_int(WindowMeta, From, Until) -> {ok, Read | no_data} when
    WindowMeta :: osc_meta_window:windowmeta(),
    From :: timestamp(),
    Until :: timestamp(),
    Read :: {timestamp(), timestamp(), [value()]}.

read_int(WindowMeta, From0, Until0) ->
    Commutator = osc_persistence_util:commutator(),
    ID = osc_meta_window:id(WindowMeta),
    Interval = osc_meta_window:interval(WindowMeta),
    Persisted = osc_meta_window:persisted(WindowMeta),
    {From1, Until1} = osc_util:adjust_query_range(
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
    case Bounds of
        not_found ->
            {ok, no_data};
        {ReadFrom, ReadUntil} ->
            {ok, Rows} = commutator:query(
                Commutator,
                [
                    {<<"id">>, equals, [ID]},
                    {<<"t">>, between, [ReadFrom, ReadUntil]}
                ]
            ),
            Read = lists:flatten(lists:foldr(
                fun(Row, Acc) ->
                    Bin = proplists:get_value(<<"v">>, Row),
                    Value = apod:inflate(Bin),
                    [Value|Acc]
                end,
                [],
                Rows
            )),
            {From2, Until2, TrimmedRead} = trim_read(
                From1,
                Until1,
                Interval,
                ReadFrom,
                Read
            ),
            {ok, {From2, Until2, TrimmedRead}}
    end.


-spec calculate_query_bounds(F, U, I, P) -> {F, U} | not_found when
    F :: timestamp(),
    U :: timestamp(),
    I :: interval(),
    P :: persisted().

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

trim_read(From, Until, Interval, ReadFrom, Read) ->
    StartIndex = erlang:max(0, (From - ReadFrom) div Interval),
    %% Until and From are both inclusive, so add one
    PointCount = ((Until - erlang:max(From, ReadFrom)) div Interval) + 1,
    %% Add one for 1-indexing
    Points = lists:sublist(Read, StartIndex + 1, PointCount),
    StartTime = ReadFrom + StartIndex * Interval,
    %% Subtract one for that nasty inclusive end again
    EndTime = StartTime + (length(Points) - 1) * Interval,
    {StartTime, EndTime, Points}.
