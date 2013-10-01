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

-record(st, {
    group,
    interval,
    count,
    persisted,
    aggregation_fun
}).

%% Minimim amount of seconds to wait before persisting to disk
-define(MIN_PERSIST_AGE, 300).

%% TODO: compress
-define(VALENCODE(V), zlib:compress(term_to_binary(V))).
-define(VALDECODE(V), binary_to_term(zlib:decompress(V))).

%% Number of bytes to store in each value
-define(MIN_CHUNK_SIZE, 1000).
-define(MAX_CHUNK_SIZE, 1024).

start() ->
    application:start(oscilloscope_cache).

stop() ->
    ok.

process(User, Name, Host, Timestamp, Value) ->
    multicast(User, Name, Host, {process, Timestamp, Value}).

read(User, Name, Host, From, Until) ->
    {Megaseconds, Seconds, _} = erlang:now(),
    QueryTime = Megaseconds * 1000000 + Seconds,
    Resolutions = multicall(User, Name, Host, get_resolution),
    Resolutions1 = lists:sort(
        fun({_, {ok, {IntervalA, _}}}, {_, {ok, {IntervalB, _}}}) ->
            IntervalA >= IntervalB
        end,
        Resolutions
    ),
    {Pid, _Resolution} = lists:foldl(
        fun({_, {ok, {Interval, Count}}}=R, Result) ->
            case QueryTime - Interval * Count < From of
                true -> R;
                false -> Result
            end
        end, hd(Resolutions1), Resolutions1),
    gen_server:call(Pid, {read, From, Until}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init({Group, {Interval, Count, Persisted}, AggregationAtom}) ->
    ok = pg2:join(Group, self()),
    AggregationFun = fun(Vals) ->
        erlang:apply(oscilloscope_cache_aggregations, AggregationAtom, [Vals])
    end,
    State = #st{
        group = Group,
        interval = Interval,
        count = Count,
        persisted = Persisted,
        aggregation_fun = AggregationFun
    },
    case read(State) of
        not_found ->
            write(State, {undefined, array:new({default, null})});
        _ ->
            ok
    end,
    {ok, State}.

handle_call(get_resolution, _From, #st{interval=I, count=C}=State) ->
    {reply, {ok, {I, C}}, State};
handle_call({read, From, Until}, _From, State) ->
    #st{interval=I, aggregation_fun=AF} = State,
    {T0, Points} = read(State),
    %% FIXME: this method leads us to read one index too far back
    %% unless `From rem I == 0`.
    StartIndex = (From - T0) div I,
    EndIndex = (Until - T0) div I,
    Acc0 = case StartIndex < 0 of
        true -> lists:duplicate(abs(StartIndex), null);
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
    RealFrom = timestamp_from_index(T0, StartIndex, I),
    RealUntil = timestamp_from_index(T0, EndIndex, I),
    Reply = [
        {from, RealFrom},
        {until, RealUntil},
        {interval, I},
        {datapoints, lists:reverse(Full)}
    ],
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
    #st{interval=I, persisted=Persisted, aggregation_fun=AF} = State,
    {T0, Points} = read(State),
    %% TODO: don't use NOW, use the last persisted time
    {Megaseconds, Seconds, _} = erlang:now(),
    Now = Megaseconds * 1000000 + Seconds,
    %% We only persist up to the epoch now - ?MIN_PERSIST_AGE
    LatestPersistTime = Now - ?MIN_PERSIST_AGE,
    PersistIndex = (LatestPersistTime - (LatestPersistTime rem I) - T0) / I,
    %% Split the array - points old enough to persist on the left, etc
    {ToPersist, ToMem} = array:foldr(
        fun(Idx, Value, {Persist, Mem}) ->
            case Idx =< PersistIndex of
                true -> {[Value|Persist], Mem};
                false -> {Persist, [Value|Mem]}
            end
        end,
        {[], []},
        Points
    ),
    %% Split the points old enough to persist in to chunks of 1k
    {Remainder, PersistChunks} = chunkify(ToPersist, AF),
    %% Persist the relevant 1k chunks, crash if there's a failure
    Timestamps = lists:map(
       fun({Index, Value}) ->
           Timestamp = timestamp_from_index(T0, Index, I),
           ok = persist(Timestamp, Value),
           Timestamp
       end,
       PersistChunks
    ),
    %% TODO: Persist what timestamps we persisted
    %% Merge extras back with the points to remain in memory
    ToMem1 = Remainder ++ ToMem,
    PersistCount = length(ToPersist) - length(Remainder),
    T1 = timestamp_from_index(T0, PersistCount, I),
    write(State, {T1, array:from_list(ToMem1)}),
    State#st{persisted=Persisted ++ lists:sort(Timestamps)}.

persist(_Timestamp, _Value) ->
    %% TODO
    ok.

-spec chunkify([number()], fun()) -> {[number()], [{integer(), binary()}]}.
chunkify(ToChunk, Aggregator) ->
    chunkify(ToChunk, 0, [], [], Aggregator).

chunkify([], _Index, ThisChunk, Chunked, _Aggregator) ->
    {ThisChunk, Chunked};
chunkify([V|Vs], Index, ThisChunk, Chunked, Aggregator) ->
    Encoded = ?VALENCODE(lists:reverse(ThisChunk)),
    {ThisChunk1, Chunked1} = case byte_size(Encoded) of
        Size when Size >= ?MIN_CHUNK_SIZE andalso Size =< ?MAX_CHUNK_SIZE ->
            {[Aggregator(V)], [{Index, Encoded}|Chunked]};
        Size when Size < ?MIN_CHUNK_SIZE ->
            {[Aggregator(V)|ThisChunk], Chunked}
    end,
    chunkify(Vs, Index + 1, ThisChunk1, Chunked1, Aggregator).

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

cache_key(#st{group=Group, interval=Interval, count=Count}) ->
    {Group, Interval, Count}.

timestamp_from_index(InitialTime, Index, Interval) ->
    InitialTime + (Index * Interval).
