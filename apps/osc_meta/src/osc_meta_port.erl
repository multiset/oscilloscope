-module(osc_meta_port).

-export([create/2]).

-include_lib("osc/include/osc_types.hrl").

-spec create(owner_id(), port()) -> ok.
create(OwnerID, Port) ->
    {ok, URL} = application:get_env(osc_meta, port_server),
    {ok, Host} = application:get_env(osc_net, host),
    {ok, "200", _Headers, Resp} = ibrowse:send_req(URL ++ "/cert", [], get),
    {JSON} = jiffy:decode(Resp),
    Cert = osc_util:get_value(<<"cert">>, JSON),
    osc_sql:named(insert_port, OwnerID, Host, Port, Cert),
    % <<"WITH cert_id AS (INSERT INTO certs (owner_id, cert) VALUES ($1, $2) RETURNING id) INSERT INTO ports (owner_id, host, port, cert_id) VALUES ($1, $2, $3, $4);">>,
    {ok, "200", _Headers, _Resp} = ibrowse:send_req(URL ++ "/refresh", [], post),
    ok.
