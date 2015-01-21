-module(osc_graphite_read_handler).

-export([init/3, terminate/3]).
-export([
    rest_init/2,
    allowed_methods/2,
    is_authorized/2,
    resource_exists/2,
    content_types_provided/2,
    to_json/2
]).

-record(st, {
    user_id,
    context,
    target,
    from,
    until,
    metricmeta
}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
    ok.

rest_init(Req0, _) ->
    {QS, Req1} = cowboy_req:qs_vals(Req0),
    State = #st{
        context = proplists:get_value(<<"context">>, QS),
        target = proplists:get_value(<<"target">>, QS),
        from = binary_to_integer(proplists:get_value(<<"from">>, QS)),
        until = binary_to_integer(proplists:get_value(<<"until">>, QS))
    },
    {ok, Req1, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

is_authorized(Req, State) ->
    osc_http:is_authorized(
        Req,
        fun(UserID) -> State#st{user_id=UserID} end,
        fun() -> State end
    ).

resource_exists(Req, State) ->
    #st{user_id = UserID, context = Context, target = Target} = State,
    MetricProps = [{<<"graphite">>, Target}],
    case Context of
        undefined ->
            {ok, UserProps} = osc_meta_user:lookup(UserID),
            UserOwnerID = proplists:get_value(owner_id, UserProps),
            case osc_meta_metric:lookup({UserOwnerID, MetricProps}) of
                {ok, MetricMeta} ->
                    {true, Req, State#st{metricmeta=MetricMeta}};
                not_found ->
                    {false, Req, State}
            end;
        OrgName ->
            case osc_meta_org:lookup(OrgName) of
                not_found ->
                    {false, Req, State};
                {ok, OrgProps} ->
                    OrgOwnerID = proplists:get_value(owner_id, OrgProps),
                    case osc_meta_metric:lookup({OrgOwnerID, MetricProps}) of
                        not_found ->
                            {false, Req, State};
                        {ok, MetricMeta} ->
                            State1 = State#st{metricmeta=MetricMeta},
                            OrgID = proplists:get_value(id, OrgProps),
                            case osc_meta_org:is_owner(OrgID, UserID) of
                                true ->
                                    {true, Req, State1};
                                false ->
                                    TPs = osc_meta_org:team_permissions(
                                        proplists:get_value(id, OrgProps),
                                        UserID
                                    ),
                                    lager:error("TPs are ~p", [TPs]),
                                    Authorized = osc_meta_util:find_prop_match(
                                        TPs,
                                        MetricProps
                                    ),
                                    {Authorized, Req, State1}
                            end
                    end
            end
    end.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, State) ->
    #st{metricmeta=MetricMeta0, from=From, until=Until} = State,
    {ok, _MetricMeta1, WindowMeta, Read} = osc:read(
        osc_meta_metric:id(MetricMeta0),
        From,
        Until
    ),
    {ReadFrom, ReadUntil, Points} = Read,
    Interval = osc_meta_window:interval(WindowMeta),
    Response = {[
        {from, ReadFrom},
        {until, ReadUntil},
        {interval, Interval},
        {points, Points}
    ]},
    {jiffy:encode(Response), Req, State}.
