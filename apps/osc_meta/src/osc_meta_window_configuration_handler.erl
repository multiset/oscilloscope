-module(osc_meta_window_configuration_handler).

-export([init/3, terminate/3]).
-export([
    rest_init/2,
    allowed_methods/2,
    is_authorized/2,
    forbidden/2,
    resource_exists/2,
    delete_resource/2,
    content_types_accepted/2,
    content_types_provided/2,
    from_json_patch/2,
    to_json/2
]).

-record(st, {
    user_id,
    owner_class,
    props
}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
    ok.

rest_init(Req, Opts) ->
    {ok, Req, #st{owner_class=proplists:get_value(owner_class, Opts)}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"DELETE">>, <<"PATCH">>], Req, State}.

is_authorized(Req, State) ->
    osc_http:is_authorized(
        Req,
        fun(UserID) -> State#st{user_id=UserID} end,
        fun() -> State end
    ).

forbidden(Req0, State0) ->
    {Req1, State1} = fetch(Req0, State0),
    #st{owner_class=OwnerClass, user_id=AuthUserID, props=Props}=State1,
    Forbidden = case Props of
        undefined ->
            false;
        _ ->
            {ClassAuthorized, ClassProps, Req1} = case OwnerClass of
                user ->
                    {UserIDBin, ReqA} = cowboy_req:binding(user_id, Req0),
                    UserID = list_to_integer(binary_to_list(UserIDBin)),
                    {ok, UserProps} = osc_meta_user:lookup(UserID),
                    %% Auth user must be identical and the user's OwnerID must
                    %% match the window config's OwnerID.
                    {UserID =:= AuthUserID, UserProps, ReqA};
                org ->
                    {OrgIDBin, ReqA} = cowboy_req:binding(org_id, Req0),
                    OrgID = list_to_integer(binary_to_list(OrgIDBin)),
                    {ok, OrgProps} = osc_meta_org:lookup(OrgID),
                    {Method, ReqB} = cowboy_req:method(ReqA),
                    MethodAuthorized = case Method of
                        <<"PATCH">> ->
                            %% Only owners can modify window configurations
                            osc_meta_org:is_owner(OrgID, AuthUserID);
                        <<"DELETE">> ->
                            %% Only owners can modify window configurations
                            osc_meta_org:is_owner(OrgID, AuthUserID);
                        <<"GET">> ->
                            %% Any member can view window configurations
                            osc_meta_org:is_member(OrgID, AuthUserID)
                    end,
                    {MethodAuthorized, OrgProps, ReqB}
            end,
            %% Verify that the group owner matches the request owner
            PropsOwnerID = proplists:get_value(owner_id, Props),
            ClassOwnerID = proplists:get_value(owner_id, ClassProps),
            if
                PropsOwnerID =:= undefined ->
                    lager:critical("Missing owner_id: ~p", [Props]),
                    true;
                ClassOwnerID =:= undefined ->
                    lager:critical("Missing owner_id: ~p", [ClassProps]),
                    true;
                not ClassAuthorized ->
                    true;
                PropsOwnerID =/= ClassOwnerID ->
                    true;
                true ->
                    false
            end
    end,
    {Forbidden, Req1, State1}.

resource_exists(Req, #st{props=undefined}=State) ->
    {false, Req, State};
resource_exists(Req, State) ->
    {true, Req, State}.

delete_resource(Req, #st{props=Props}=State) ->
    ok = osc_meta_window_configuration:delete(proplists:get_value(id, Props)),
    {true, Req, State}.

content_types_accepted(Req, State) ->
    {
        [{{<<"application">>, <<"json-patch+json">>, '*'}, from_json_patch}],
        Req,
        State
    }.

from_json_patch(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req0),
    {ok, {Operation, Path, Value}} = osc_http:parse_json_patch(Body),
    #st{props=Props0} = State,
    case apply_patch(Operation, Path, Value, Props0) of
        {ok, Props1} ->
            {true, Req1, State#st{props=Props1}};
        error ->
            {false, Req1, State}
    end.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, State) ->
    #st{props=Props0} = State,
    {value, {windows, Windows0}, Props1} = lists:keytake(
        windows,
        1,
        Props0
    ),
    Windows1 = [{W} || W <- Windows0],
    {value, {tags, Tags}, Props2} = lists:keytake(tags, 1, Props1),
    EJSON = {[{windows, Windows1}, {tags, {Tags}}] ++ Props2},
    {jiffy:encode(EJSON), Req, State}.

fetch(Req0, State) ->
    {WindowIDBin, Req1} = cowboy_req:binding(window_id, Req0),
    WindowID = list_to_integer(binary_to_list(WindowIDBin)),
    case osc_meta_window_configuration:lookup(WindowID) of
        not_found ->
            {Req1, State};
        {ok, WindowProps} ->
            {Req1, State#st{props=WindowProps}}
    end.

apply_patch(<<"add">>, [<<"windows">>], {WindowProps}, GroupProps0) ->
    GroupID = proplists:get_value(id, GroupProps0),
    Type = osc_meta_util:parse_window_type(
        proplists:get_value(<<"type">>, WindowProps)
    ),
    Aggregation = osc_meta_util:parse_window_aggregation(
        proplists:get_value(<<"aggregation">>, WindowProps)
    ),
    Interval = proplists:get_value(<<"interval">>, WindowProps),
    Count = proplists:get_value(<<"count">>, WindowProps),
    %% Validation!
    true = is_integer(Interval),
    true = is_integer(Count),
    true = Type =:= rectangular,
    true = lists:member(Aggregation, [avg]),
    WindowConfig = {
        Type,
        Aggregation,
        Interval,
        Count
    },
    ok = osc_meta_window_configuration:add_window(GroupID, WindowConfig),
    {ok, GroupProps1} = osc_meta_window_configuration:lookup(GroupID),
    {ok, GroupProps1};
apply_patch(<<"remove">>, [<<"windows">>], {WindowProps}, GroupProps0) ->
    GroupID = proplists:get_value(id, GroupProps0),
    WindowID = proplists:get_value(<<"id">>, WindowProps),
    ok = osc_meta_window_configuration:delete_window(GroupID, WindowID),
    {ok, GroupProps1} = osc_meta_window_configuration:lookup(GroupID),
    {ok, GroupProps1};
apply_patch(Op, Path, _, Props) ->
    lager:error(
        "Got an unknown patch attempt: ~p, ~p for window group ~p",
        [Op, Path, Props]
    ),
    error.
