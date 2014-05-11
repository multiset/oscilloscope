-module(oscilloscope).

-export([
    start/0,
    stop/0,
    update/2,
    update/3,
    read/3,
    read/4
]).

-include("oscilloscope.hrl").
-include("oscilloscope_types.hrl").

start() ->
    application:start(oscilloscope).

stop() ->
    ok.

update(Metric, Points) ->
    update(Metric, Points, []).

update(Metric, Points, Opts) ->
    Timeout = proplists:get_value(timeout, Opts, ?DEFAULT_TIMEOUT),
    {ok, ReqID} = oscilloscope_update_fsm:update(Metric, Points, Opts),
    wait_and_reply(ReqID, Timeout).

read(Metric, From, Until) ->
    read(Metric, From, Until, []).

read(Metric, From, Until, Opts) ->
    Timeout = proplists:get_value(timeout, Opts, ?DEFAULT_TIMEOUT),
    {ok, ReqID} = oscilloscope_read_fsm:read(Metric, From, Until, Opts),
    wait_and_reply(ReqID, Timeout).

wait_and_reply(ReqID, Timeout) ->
    receive {ReqID, Response} ->
        Response
    after Timeout ->
        {error, timeout}
    end.
