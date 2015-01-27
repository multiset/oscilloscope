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
    props
}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
    ok.

rest_init(Req, Opts) ->
    {ok, Req, #st{}}.

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
    #st{user_id=AuthUserID, props=Props}=State1,
    {Forbidden, Req4} = case Props of
        undefined ->
            false;
        _ ->
            {OrgIDBin, Req2} = cowboy_req:binding(org_id, Req1),
            OrgID = list_to_integer(binary_to_list(OrgIDBin)),
            {Method, Req3} = cowboy_req:method(Req2),
            Authorized = case Method of
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
            {not Authorized, Req3}
    end,
    {Forbidden, Req4, State1}.

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
apply_patch(<<"replace">>, [<<"priority">>], Priority, GroupProps0) ->
    true = is_integer(Priority),
    GroupID = proplists:get_value(id, GroupProps0),
    ok = osc_meta_window_configuration:set_priority(GroupID, Priority),
    {ok, GroupProps1} = osc_meta_window_configuration:lookup(GroupID),
    {ok, GroupProps1};
apply_patch(Op, Path, _, Props) ->
    lager:error(
        "Got an unknown patch attempt: ~p, ~p for window group ~p",
        [Op, Path, Props]
    ),
    error.
