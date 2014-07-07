-module(oscilloscope_auth_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("oscilloscope_auth.hrl").


start(_StartType, _StartArgs) ->
    oscilloscope_auth_sup:start_link().

stop(_State) ->
    ok.
