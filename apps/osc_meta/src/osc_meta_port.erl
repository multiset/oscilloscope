-module(osc_meta_port).

-export([create/2, start/2]).

-include_lib("osc/include/osc_types.hrl").

-spec create(owner_id(), port()) -> ok.
create(OwnerID, Port) ->
    {ok, Host} = application:get_env(osc_net, host),
    case application:get_env(osc_meta, use_stunnel) of
        {ok, true} ->
            {ok, URL} = application:get_env(osc_meta, port_server),
            {ok, "200", _Headers, Resp} = ibrowse:send_req(
                URL ++ "/cert",
                [],
                get
            ),
            {JSON} = jiffy:decode(Resp),
            Cert = osc_util:get_value(<<"cert">>, JSON),
            {ok, 1} = osc_sql:named(
                insert_port_and_cert,
                [OwnerID, Host, Port, Cert]
            ),
            {ok, "200", _Headers, _Resp} = ibrowse:send_req(
                URL ++ "/refresh",
                [],
                post
            );
        {ok, false} ->
            {ok, 1} = osc_sql:named(
                insert_port,
                [OwnerID, Host, Port]
            )
    end,
    ok.

-spec start(owner_id(), port()) -> ok.
start(OwnerID, Port) ->
    {ok, _Pid} = ranch:start_listener(
        OwnerID,
        1,
        ranch_tcp,
        [{port, Port}],
        osc_net_tcp,
        [{parser, fun osc_net_protocols:graphite/1}]
    ),
    ok.
