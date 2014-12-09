-module(osc).

-export([
    start/0,
    stop/0,
    update/2,
    read/3
]).

-include("osc_types.hrl").

start() ->
    application:start(osc).


stop() ->
    ok.


-spec update(Metric, Points) -> ok when
    Metric :: metric(),
    Points :: [{timestamp(), value()}].

update(Metric, Points) ->
    osc_cache:update(Metric, Points).


-spec read(Metric, From, Until) -> {ok, MetricMeta, WindowMeta, Read} when
    Metric :: metric(),
    From :: timestamp(),
    Until :: timestamp(),
    MetricMeta :: osc_meta_metric:metricmeta(),
    WindowMeta :: osc_meta_window:windowmeta(),
    Read :: {timestamp(), timestamp(), [value()]}.

read(Metric, From, Until) ->
    {ok, MetricMeta, WindowMeta, CacheRead} = osc_cache:read(
        Metric,
        From,
        Until
    ),
    {ok, PersistentRead} = osc_persistence:read(WindowMeta, From, Until),
    Interval = osc_meta_window:interval(WindowMeta),
    MergedRead = merge_reads(
        From,
        Until,
        Interval,
        CacheRead,
        PersistentRead
    ),
    {ok, MetricMeta, WindowMeta, MergedRead}.


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
            lists:duplicate((Until1 - From1) div Interval, null);
        {{PFrom, PUntil, PData}, no_data} ->
            lists:append([
                lists:duplicate((PFrom - From1) div Interval, null),
                PData,
                lists:duplicate((Until1 - PUntil) div Interval, null)
            ]);
        {no_data, {CFrom, CUntil, CData}} ->
            lists:append([
                lists:duplicate((CFrom - From1) div Interval, null),
                CData,
                lists:duplicate((Until1 - CUntil) div Interval, null)
            ]);
        {{_, PUntil, PData}, {CFrom, CUntil, CData}} ->
            lists:append([
                lists:duplicate((CFrom - From1) div Interval, null),
                PData,
                lists:duplicate((CFrom - PUntil) div Interval, null),
                CData,
                lists:duplicate((Until1 - CUntil) div Interval, null)
            ])
    end,
    {From1, Until1, Points}.
