-module(osc).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    start/0,
    stop/0,
    update/2,
    read/3
]).

-include_lib("osc/include/osc_types.hrl").

start() ->
    application:start(osc).


stop() ->
    ok.


-spec update(Metric, Points) -> ok | not_found when
    Metric :: metric_id(),
    Points :: [{timestamp(), value()}].

update(Metric, Points) ->
    case osc_cache:find(Metric) of
        not_found ->
            not_found;
        {ok, Pid} ->
            osc_cache:update(Pid, Points)
    end.


-spec read(Metric, From, Until) -> {ok, MMeta, WMeta, Read} | not_found when
    Metric :: metric_id(),
    From :: timestamp(),
    Until :: timestamp(),
    MMeta :: osc_meta_metric:metricmeta(),
    WMeta :: osc_meta_window:windowmeta(),
    Read :: {timestamp(), timestamp(), [value()]}.

read(Metric, From0, Until0) ->
    mstat:increment_counter([osc, reads, count]),
    case osc_cache:find(Metric) of
        not_found ->
            not_found;
        {ok, Pid} ->
            case osc_cache:read(Pid, From0, Until0) of
                not_ready ->
                    not_found;
                {ok, MetricMeta, WindowMeta, CacheRead} ->
                    {ok, PersistentReads} = osc_persistence:read(
                        WindowMeta,
                        From0,
                        Until0
                    ),
                    Interval = osc_meta_window:interval(WindowMeta),
                    {From1, Until1} = osc_util:adjust_query_range(
                        From0,
                        Until0,
                        Interval
                    ),
                    PointsRead = merge_reads(
                        From1,
                        Until1,
                        Interval,
                        [CacheRead|PersistentReads]
                    ),
                    mstat:increment_counter([osc, reads, successful]),
                    mstat:increment_counter(
                        [osc, reads, points],
                        (Until1 - From1) div Interval
                    ),
                    {ok, MetricMeta, WindowMeta, {From1, Until1, PointsRead}}
            end
    end.


-spec merge_reads(From, Until, Interval, Reads) -> Points when
    From :: timestamp(),
    Until :: timestamp(),
    Interval :: interval(),
    Reads :: [Read],
    Read :: {timestamp(), timestamp(), [value()]} | no_data,
    Points :: [value()].

merge_reads(From, Until, Interval, Reads) ->
    merge_reads(From, Until, Interval, Reads, []).

merge_reads(From, Until, Interval, [], Acc) when From =< Until ->
    Tail = ((Until - From) div Interval) + 1,
    Acc ++ lists:duplicate(Tail, undefined);
merge_reads(_, _, _, [], Acc) ->
    Acc;
merge_reads(From, Until, Interval, [no_data|Rs], Acc) ->
    merge_reads(From, Until, Interval, Rs, Acc);
merge_reads(From, Until, Interval, [{RF, _, _}|Rs], Acc) when RF < From ->
    %% This read overlapped with the previous read - skip it and move on.
    lager:error("merge_reads encountered overlapping reads; RF = ~p", [RF]),
    merge_reads(From, Until, Interval, Rs, Acc);
merge_reads(From, Until, Interval, [{RF, _, _}|Rs], Acc) when RF > Until ->
    %% This read has no datapoints in the requested range - skip it and move on.
    lager:error("merge_reads encountered irrelevant read; RF = ~p", [RF]),
    merge_reads(From, Until, Interval, Rs, Acc);
merge_reads(From, Until, Interval, [{RF, _, _}|_]=Rs, Acc0) when RF > From ->
    %% Fill in the gap with undefineds and continue with the same read
    Acc1 = Acc0 ++ lists:duplicate((RF - From) div Interval, undefined),
    merge_reads(RF, Until, Interval, Rs, Acc1);
merge_reads(_, Until, Interval, [{_, RU, Points}|Rs], Acc) when RU < Until ->
    merge_reads(RU + Interval, Until, Interval, Rs, Acc ++ Points);
merge_reads(_, Until, Interval, [{RF, _, Points}|_], Acc) ->
    %% This read extends either to or past the end of the requested read. This
    %% has two implications: first: we don't have to recurse, since we know
    %% we've read everything requested, and second: we can't append the full
    %% read to the Acc, since that might result in returning too many points.
    Acc ++ lists:sublist(Points, ((Until - RF) div Interval) + 1).

