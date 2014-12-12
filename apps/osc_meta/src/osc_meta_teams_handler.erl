-module(osc_meta_teams_handler).

-export([init/3, terminate/3]).
-export([
    rest_init/2,
    allowed_methods/2,
    is_authorized/2,
    forbidden/2,
    content_types_accepted/2,
    content_types_provided/2,
    from_json/2,
    to_json/2
]).

-record(st, {
    user_id,
    org_id
}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
    ok.

rest_init(Req, _State) ->
    {ok, Req, #st{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

is_authorized(Req, State) ->
    osc_http:is_authorized(
        Req,
        fun(UserID) -> State#st{user_id=UserID} end,
        fun() -> State end
    ).

forbidden(Req0, State0) ->
    {OrgIDBin, Req1} = cowboy_req:binding(orgid, Req0),
    OrgID = list_to_integer(binary_to_list(OrgIDBin)),
    State1 = State0#st{org_id=OrgID},
    {Method, Req2} = cowboy_req:method(Req1),
    Forbidden = case Method of
        <<"POST">> ->
            %% Creating a new team requires org ownership
            IsOwner = osc_meta_org:is_owner(OrgID, State1#st.user_id),
            not IsOwner;
        <<"GET">> ->
            %% Listing teams only requires membership
            IsMember = osc_meta_org:is_member(OrgID, State1#st.user_id),
            not IsMember
    end,
    {Forbidden, Req2, State1}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

from_json(Req0, State) ->
    {ok, JSONBody, Req1} = cowboy_req:body(Req0),
    {Body} = jiffy:decode(JSONBody),
    TeamName = proplists:get_value(<<"name">>, Body),
    true = TeamName =/= <<>>,
    {ok, TeamID} = osc_meta_team:create(State#st.org_id, TeamName),
    %% TODO: Is the user automatically added to the team?
    Req2 = cowboy_req:set_resp_header(
        <<"Location">>,
        [<<"/orgs/">>, State#st.org_id, <<"/teams/">>, integer_to_list(TeamID)],
        Req1
    ),
    {true, Req2, State}.

to_json(Req, State) ->
    Teams = lists:map(
        fun({ID, Name, Members}) ->
            {[
                {id, ID},
                {name, Name},
                {members, Members},
                {metrics, 0} %% TODO
            ]}
        end,
        osc_meta_org:teams(State#st.org_id)
    ),
    {jiffy:encode(Teams), Req, State}.
