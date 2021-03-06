-module(osc_graphite_find_handler).

-export([init/3, terminate/3]).
-export([
    rest_init/2,
    allowed_methods/2,
    is_authorized/2,
    content_types_provided/2,
    to_json/2
]).

-record(st, {
    user_id
}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
    ok.

rest_init(Req, _State) ->
    {ok, Req, #st{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

is_authorized(Req, State) ->
    osc_http:is_authorized(
        Req,
        fun(UserID) -> State#st{user_id=UserID} end,
        fun() -> State end
    ).

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req0, State) ->
    {Query, Req1} = cowboy_req:qs_val(<<"q">>, Req0),
    {OrgName, Req2} = cowboy_req:qs_val(<<"org_name">>, Req1),
    %% Split the incoming query on literal . characters, which arrive
    %% double-escaped. Graphite builds a tree based on a succession of
    %% .-separated segments, so the query is only looking for the first element
    %% after the ., effectively.
    QuerySize = length(binary:split(Query, <<"\\.">>, [global])),
    UserID = State#st.user_id,
    {ok, OrgProps} = osc_meta_org:lookup(OrgName),
    OrgID = proplists:get_value(id, OrgProps),
    UnfilteredMetrics = osc_meta_metric:search(
        OrgID,
        [{<<"graphite">>, Query}]
    ),
    FilteredMetrics = case osc_meta_org:is_owner(OrgID, UserID) of
        true ->
            UnfilteredMetrics;
        false ->
            TeamPermissions = osc_meta_org:team_permissions(
                OrgID,
                UserID
            ),
            lists:filter(
                fun({_, MetricProps}) ->
                    osc_meta_util:find_prop_match(
                        TeamPermissions,
                        MetricProps
                    )
                end,
                UnfilteredMetrics
            )
    end,
    Response = lists:usort(lists:map(
        fun({_, MetricProps}) ->
            Name = proplists:get_value(<<"graphite">>, MetricProps),
            NameParts = binary:split(Name, <<".">>, [global]),
            case length(NameParts) of
                Size when Size == QuerySize ->
                    {Name, leaf};
                Size when Size >= QuerySize ->
                    SubName = osc_util:binary_join(
                        lists:sublist(NameParts, 1, QuerySize),
                        <<".">>
                    ),
                    {SubName, branch}
            end
        end,
        FilteredMetrics
    )),
    {jiffy:encode({Response}), Req2, State}.
