-module(osc_meta_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    {ok, _, Ports} = osc_sql:named(get_all_ports, []),
    lists:map(fun({OwnerID, Port}) ->
        ok = osc_meta_port:start(OwnerID, Port)
    end, Ports),
    ok.

stop(_State) ->
    ok.
