-module(osc).

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

read(Metric, From, Until) ->
    mstat:increment_counter([osc, reads, count]),
    case osc_cache:find(Metric) of
        not_found ->
            not_found;
        {ok, Pid} ->
            case osc_cache:read(Pid, From, Until) of
                not_ready ->
                    not_found;
                {ok, MetricMeta, WindowMeta, CacheRead} ->
                    {ok, PersistentRead} = osc_persistence:read(
                        WindowMeta,
                        From,
                        Until
                    ),
                    Interval = osc_meta_window:interval(WindowMeta),
                    MergedRead = merge_reads(
                        From,
                        Until,
                        Interval,
                        CacheRead,
                        PersistentRead
                    ),
                    mstat:increment_counter([osc, reads, successful]),
                    mstat:increment_counter(
                        [osc, reads, points],
                        length(MergedRead)
                    ),
                    {ok, MetricMeta, WindowMeta, MergedRead}
            end
    end.


-spec merge_reads(From, Until, Interval, CRead, PRead) -> Read when
    From :: timestamp(),
    Until :: timestamp(),
    Interval :: interval(),
    CRead :: {timestamp(), timestamp(), [value()]} | no_data,
    PRead :: {timestamp(), timestamp(), [value()]} | no_data,
    Read :: {timestamp(), timestamp(), [value()]}.

merge_reads(From0, Until0, Interval, CRead, PRead) ->
    {From1, Until1} = osc_util:adjust_query_range(
        From0,
        Until0,
        Interval
    ),
    Points = case {PRead, CRead} of
        {no_data, no_data} ->
            mstat:increment_counter([osc, reads, undefined]),
            lists:duplicate((Until1 - From1) div Interval, undefined);
        {{PFrom, PUntil, PData}, no_data} ->
            mstat:increment_counter([osc, reads, persistent_only]),
            lists:append([
                lists:duplicate((PFrom - From1) div Interval, undefined),
                PData,
                lists:duplicate((Until1 - PUntil) div Interval, undefined)
            ]);
        {no_data, {CFrom, CUntil, CData}} ->
            mstat:increment_counter([osc, reads, cached_only]),
            lists:append([
                lists:duplicate((CFrom - From1) div Interval, undefined),
                CData,
                lists:duplicate((Until1 - CUntil) div Interval, undefined)
            ]);
        {{_, PUntil, PData}, {CFrom, CUntil, CData}} ->
            mstat:increment_counter([osc, reads, persistent_and_cached]),
            lists:append([
                lists:duplicate((CFrom - From1) div Interval, undefined),
                PData,
                lists:duplicate((CFrom - PUntil) div Interval, undefined),
                CData,
                lists:duplicate((Until1 - CUntil) div Interval, undefined)
            ])
    end,
    {From1, Until1, Points}.
