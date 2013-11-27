-module(oscilloscope_cache).

-export([
    start/0,
    stop/0,
    process/5,
    read/5
]).

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

-record(st, {
    resolution_id :: resolution_id(),
    group :: group(),
    interval :: interval(),
    count :: count(),
    persisted :: persisted(),
    aggregation_fun :: fun(),
    commutator
}).

%% Minimim amount of seconds to wait before persisting to disk
-define(MIN_PERSIST_AGE, 300).

%% TODO: try zlib:zip instead
-define(VALENCODE(V), zlib:compress(term_to_binary(V))).
-define(VALDECODE(V), binary_to_term(zlib:uncompress(V))).

%% Number of bytes to store in each value
-define(MIN_CHUNK_SIZE, 1000).
-define(MAX_CHUNK_SIZE, 1024).

%% Dynamo/commutator things. Should go in config
-define(DYNAMO_TABLE, <<"oscilloscope-test">>).
-define(DYNAMO_SCHEMA, [{<<"id">>, n}, {<<"t">>, n}, {<<"v">>, b}]).
-define(DYNAMO_REGION, "us-east-1").
-define(DYNAMO_ACCESSKEY, "AKIAJNCK2CQXCEEM6MDA").
-define(DYNAMO_SECRETKEY, "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").

start() ->
    application:start(oscilloscope_cache).

stop() ->
    ok.

process(User, Name, Host, Timestamp, Value) ->
    multicast(User, Name, Host, {process, Timestamp, Value}).

read(User, Name, Host, From, Until) ->
    Resolutions = multicall(User, Name, Host, get_metadata),
    [{Pid0, _}|Rs] = lists:sort(
        fun({_, {ok, MetaA}}, {_, {ok, MetaB}}) ->
            IntervalA = proplists:get_value(interval, MetaA),
            IntervalB = proplists:get_value(interval, MetaB),
            IntervalA >= IntervalB
        end,
        Resolutions
    ),
    Pid = lists:foldl(
        fun({P, {ok, Meta}}, Acc) ->
            case proplists:get_value(earliest_time, Meta) < From of
                true -> P;
                false -> Acc
            end
        end, Pid0, Rs),
    gen_server:call(Pid, {read, From, Until}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init({Group, ResolutionId, Interval, Count, Persisted, AggregationAtom}) ->
    ok = pg2:join(Group, self()),
    AggregationFun = fun(Vals) ->
        erlang:apply(oscilloscope_cache_aggregations, AggregationAtom, [Vals])
    end,
    {ok, C} = commutator:connect(
        ?DYNAMO_TABLE,
        ?DYNAMO_SCHEMA,
        ?DYNAMO_REGION,
        ?DYNAMO_ACCESSKEY,
        ?DYNAMO_SECRETKEY
    ),
    State = #st{
        resolution_id = ResolutionId,
        group = Group,
        interval = Interval,
        count = Count,
        persisted = Persisted,
        aggregation_fun = AggregationFun,
        commutator = C
    },
    case read(State) of
        not_found ->
            %% TODO: get T0 (undefined here) from persistent store
            write(State, {undefined, array:new({default, null})});
        _ ->
            ok
    end,
    {ok, State}.

handle_call(get_metadata, _From, #st{interval=Interval, count=Count}=State) ->
    {T0, Points} = read(State),
    {EarliestTime, LatestTime} = case T0 of
        undefined ->
            {undefined, undefined};
        _ ->
            L = timestamp_from_index(T0, array:size(Points) - 1, Interval),
            E = L - Interval * Count,
            {E, L}
    end,
    Metadata = [
        {latest_time, LatestTime},
        {earliest_time, EarliestTime},
        {interval, Interval},
        {count, Count},
        {aggregation_fun, State#st.aggregation_fun}
    ],
    {reply, {ok, Metadata}, State};
handle_call({read, From, Until}, _From, State) ->
    #st{
        resolution_id=Id,
        interval=I,
        persisted=Persisted,
        aggregation_fun=AF,
        commutator=Commutator
    } = State,
    {T0, Points} = read(State),
    Reply = case T0 of
        undefined ->
            PointCount = erlang:trunc((Until - From) / I),
            Nulls = lists:duplicate(PointCount, null),
            [{from, From}, {until, Until}, {interval, I}, {datapoints, Nulls}];
        _ ->
            %% FIXME: this method leads us to read one index too far back
            %% unless `From rem I == 0`.
            StartIndex = (From - T0) div I,
            EndIndex = (Until - T0) div I,
            RealFrom = timestamp_from_index(T0, StartIndex, I),
            RealUntil = timestamp_from_index(T0, EndIndex, I),
            Acc0 = case StartIndex < 0 of
                true -> persistent_read(Id, Commutator, From, Until, Persisted);
                false -> []
            end,
            InRange = array:foldl(
                fun(Index, Vs, Acc) ->
                    case Index >= StartIndex andalso Index =< EndIndex of
                        true -> [AF(Vs)|Acc];
                        false -> Acc
                    end
                end, Acc0, Points
            ),
            MissingTail = (EndIndex - StartIndex) - (length(InRange) - 1),
            Full = case MissingTail > 0 of
                true ->
                    lists:duplicate(MissingTail, null) ++ InRange;
                false ->
                    InRange
            end,
            [
                {from, RealFrom},
                {until, RealUntil},
                {interval, I},
                {datapoints, lists:reverse(Full)}
            ]
    end,
    {reply, {ok, Reply}, State};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, error, State}.

handle_cast({process, Timestamp, Value}, State) ->
    #st{interval=Interval, persisted=Persisted} = State,
    Timestamp1 = Timestamp - (Timestamp rem Interval),
    %% We claim a ?MIN_PERSIST_AGE maximum age
    %% but that's enforced by not persisting newer points.
    %% In practice we'll accept anything that's newer than the last persist.
    %% ^^ I have no idea what this comment means, but I suspect it's sensible
    LastPersist = case Persisted of
        [] -> 0;
        Persisted -> lists:last(Persisted)
    end,
    %% TODO: check if timestamp is too old
    if
        Timestamp1 > LastPersist ->
            {T0, OldPoints} = read(State),
            {T1, Index} = case T0 of
                undefined ->
                    {Timestamp1, 0};
                _ ->
                    {T0, (Timestamp1 - T0) div Interval}
            end,
            NewPoints = case array:get(Index, OldPoints) of
                null ->
                    array:set(Index, [Value], OldPoints);
                V when is_list(V) ->
                    array:set(Index, [Value|V], OldPoints);
                V ->
                    array:set(Index, [Value, V], OldPoints)
            end,
            write(State, {T1, NewPoints});
        true ->
            ok
    end,
    {noreply, State, 0};
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(timeout, State) ->
    {noreply, maybe_persist_points(State)};
handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% TODO: this is a disaster
maybe_persist_points(#st{}=State) ->
    #st{
        resolution_id=Id,
        interval=Interval,
        persisted=Persisted,
        aggregation_fun=AggregationFun,
        commutator=Commutator
    } = State,
    {T0, Points} = read(State),
    %% We only persist up to the newest point - ?MIN_PERSIST_AGE
    PersistIndex = erlang:trunc(array:size(Points) - ?MIN_PERSIST_AGE/Interval),
    case PersistIndex =< 0 of
        true ->
            State;
        false ->
            %% Split the array
            {ToPersist, ToMem} = divide_array(Points, PersistIndex),
            %% Split the points old enough to persist in to chunks
            {Remainder, PersistChunks} = chunkify(
                ToPersist,
                AggregationFun,
                ?MIN_CHUNK_SIZE,
                ?MAX_CHUNK_SIZE
            ),
            %% Persist the relevant chunks, crash if there's a failure
            Timestamps = lists:map(
                fun({Index, Value}) ->
                    Timestamp = timestamp_from_index(T0, Index, Interval),
                    ok = persist(Id, Timestamp, Value, Commutator),
                    Timestamp
                end,
                PersistChunks
            ),
            %% Merge extras back with the points to remain in memory
            %% Increment the start timestamp for the number of points persisted
            PersistCount = length(ToPersist) - length(Remainder),
            T1 = timestamp_from_index(T0, PersistCount, Interval),
            write(State, {T1, array:from_list(Remainder ++ ToMem)}),
            State#st{persisted=Persisted ++ lists:sort(Timestamps)}
    end.

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


persist(Id, Timestamp, Value, Commutator) ->
    {ok, true} = commutator:put_item(
        Commutator,
        [{<<"id">>, Id}, {<<"t">>, Timestamp}, {<<"v">>, Value}]
    ),
    ok = oscilloscope_sql_metrics:update_persisted(Id, Timestamp),
    ok.

persistent_read(_Id, _Commutator, _From, _Until, []) ->
    [];
persistent_read(Id, Commutator, From, Until, Persisted) ->
    StartTime = calculate_starttime(From, Persisted),
    EndTime = calculate_endtime(Until, Persisted),
    {ok, Rows} = commutator:query(
        Commutator,
        [{<<"id">>, eq, [Id]}, {<<"t">>, between, [StartTime, EndTime]}],
        100000 %% TODO: paginate, and teach commutator to accept no limit
    ),
    lists:flatten([?VALDECODE(proplists:get_value(<<"v">>, I)) || I <- Rows]).

calculate_starttime(T, Ts) ->
    {Earlier, Later} = lists:partition(fun(I) -> I =< T end, Ts),
    case Earlier of
        [] -> hd(Later);
        _Any -> lists:last(Earlier)
    end.

calculate_endtime(T, Ts) ->
    {Later, _Earlier} = lists:partition(fun(I) -> I >= T end, Ts),
    case Later of
        [] -> T;
        _Any -> hd(Later)
    end.

-spec chunkify([[number()]], fun(), integer(), integer()) ->
  {[[number()]], [{integer(), binary()}]}.
chunkify(Values, Aggregator, ChunkMin, ChunkMax) ->
    {_, Remainder, Chunks} = lists:foldl(
        fun(Value, {Count, Pending, Chunks}) ->
            Pending1 = [Value|Pending],
            %% TODO: lots of lists:reverse here!
            Encoded = ?VALENCODE(lists:reverse(Pending1)),
            case byte_size(Encoded) of
                Size when Size >= ChunkMin andalso Size =< ChunkMax ->
                    {Count + length(Pending1), [], [{Count, Encoded}|Chunks]};
                Size when Size < ChunkMin ->
                    {Count, Pending1, Chunks}
            end
        end,
        {0, [], []},
        lists:map(Aggregator, Values)
    ),
    RemainingValues = lists:sublist(
        Values, length(Values) - length(Remainder) + 1, length(Values)),
    {RemainingValues, lists:reverse(Chunks)}.

multicast(User, Name, Host, Msg) ->
    Pids = get_pids(User, Name, Host),
    lists:map(fun(P) -> gen_server:cast(P, Msg) end, Pids).

multicall(User, Name, Host, Msg) ->
    Pids = get_pids(User, Name, Host),
    lists:map(fun(P) -> {P, gen_server:call(P, Msg)} end, Pids).

get_pids(User, Name, Host) ->
    Group = {User, Name, Host},
    case pg2:get_members(Group) of
        {error, {no_such_group, Group}} ->
            {ok, _Pid} = oscilloscope_cache_sup:spawn_cache(Group),
            pg2:get_members(Group);
        [] ->
            {ok, _Pid} = oscilloscope_cache_sup:spawn_cache(Group),
            pg2:get_members(Group);
        P ->
            P
    end.

read(State) ->
    Key = cache_key(State),
    case erp:q(["GET", term_to_binary(Key)]) of
        {ok, undefined} -> not_found;
        {ok, Value} -> ?VALDECODE(Value)
    end.

write(State, Value) ->
    Key = cache_key(State),
    {ok, <<"OK">>} = erp:q(["SET", term_to_binary(Key), ?VALENCODE(Value)]),
    ok.

cache_key(#st{resolution_id=Resolution}) ->
    Resolution.

timestamp_from_index(InitialTime, Index, Interval) ->
    InitialTime + (Index * Interval).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

divide_array_test() ->
    ?assertEqual({[], []}, divide_array(array:new(), 4)),
    ?assertEqual(
        {[1, 2], [3, 4]},
        divide_array(array:from_list([1, 2, 3, 4]), 2)
    ),
    ?assertEqual(
        {[1, 2, 3, 4], []},
        divide_array(array:from_list([1, 2, 3, 4]), 7)
    ).

calculate_starttime_test() ->
    ?assertEqual(2, calculate_starttime(3, [2, 4, 6, 8])),
    ?assertEqual(2, calculate_starttime(1, [2, 4, 6, 8])).

calculate_endtime_test() ->
    ?assertEqual(4, calculate_endtime(3, [2, 4, 6, 8])),
    ?assertEqual(9, calculate_endtime(9, [2, 4, 6, 8])).

chunkify_test() ->
    %% No chunking
    Input = [[1], [2], [3, 5]],
    ?assertEqual(
        {Input, []},
        chunkify(
            Input,
            fun oscilloscope_cache_aggregations:avg/1,
            1000000,
            1000000
        )
    ),
    %% Chunking each value
    {Remainder, Chunked} = chunkify(
        Input,
        fun oscilloscope_cache_aggregations:avg/1,
        0,
        1000000
    ),
    Decoded = lists:map(fun({I, V}) -> {I, ?VALDECODE(V)} end, Chunked),
    ?assertEqual([], Remainder),
    ?assertEqual([{0, [1.0]}, {1, [2.0]}, {2, [4.0]}], Decoded),
    %% Chunking multiple values together
    Input1 = [[float(I)] || I <- lists:seq(0, 21)],
    {Remainder1, Chunked1} = chunkify(
        Input1,
        fun oscilloscope_cache_aggregations:avg/1,
        50,
        75
    ),
    Decoded1 = lists:map(fun({I, V}) -> {I, ?VALDECODE(V)} end, Chunked1),
    ?assertEqual([], Remainder1),
    ?assertEqual([
        {0, [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0]},
        {8, [8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0]},
        {15, [15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0]}
    ], Decoded1).

-endif.
