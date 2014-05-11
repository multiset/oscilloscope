-module(oscilloscope_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case oscilloscope_sup:start_link() of
        {ok, Pid} ->
            riak_core:register(oscilloscope, [
                {vnode_module, oscilloscope_vnode}
                %% TODO - Not really sure what these are
                %% {health_check, {?MODULE, check_kv_health, []}},
                %% {bucket_validator, riak_kv_bucket},
                %% {stat_mod, riak_kv_stat}
            ]),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
