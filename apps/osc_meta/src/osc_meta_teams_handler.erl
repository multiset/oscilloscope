-module(osc_meta_teams_handler).

-export([init/3, terminate/3]).
-export([
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    from_json/2,
    to_json/2
]).

-record(st, {

}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
    ok.

rest_init(Req, _State) ->
    {ok, Req, #st{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

from_json(Req0, State) ->
    {ok, JSONBody, Req1} = cowboy_req:body(Req0),
    {Body} = jiffy:decode(JSONBody),
    TeamName = proplists:get_value(<<"name">>, Body),
    true = TeamName =/= <<>>,
    {OrgIDBin, Req2} = cowboy_req:binding(orgid, Req1),
    OrgID = list_to_integer(binary_to_list(OrgIDBin)),
    {ok, TeamID} = osc_meta_team:create(OrgID, TeamName),
    %% TODO: Is the user automatically added to the team?
    Req3 = cowboy_req:set_resp_header(
        <<"Location">>,
        [<<"/orgs/">>, OrgIDBin, <<"/teams/">>, integer_to_list(TeamID)],
        Req2
    ),
    {true, Req3, State}.

to_json(Req0, State) ->
    {OrgIDBin, Req1} = cowboy_req:binding(orgid, Req0),
    OrgID = list_to_integer(binary_to_list(OrgIDBin)),
    Teams = lists:map(
        fun({ID, Name, Members}) ->
            {[
                {id, ID},
                {name, Name},
                {members, Members},
                {metrics, 0} %% TODO
            ]}
        end,
        osc_meta_org:teams(OrgID)
    ),
    {jiffy:encode(Teams), Req1, State}.
