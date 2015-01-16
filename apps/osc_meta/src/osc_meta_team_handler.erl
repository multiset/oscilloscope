-module(osc_meta_team_handler).

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
    org_id,
    team_props
}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
    ok.

rest_init(Req0, _) ->
    State0 = #st{},
    {Bindings, Req1} = cowboy_req:bindings(Req0),
    IDs = {
        proplists:get_value(org_id, Bindings),
        proplists:get_value(team_id, Bindings)
    },
    case IDs of
        {undefined, _} ->
            {ok, Req1, State0};
        {_, undefined} ->
            {ok, Req1, State0};
        {OrgIDBin, TeamIDBin} ->
            OrgID = list_to_integer(binary_to_list(OrgIDBin)),
            TeamID = list_to_integer(binary_to_list(TeamIDBin)),
            case osc_meta_team:lookup(TeamID) of
                not_found ->
                    {ok, Req1, State0};
                {ok, TeamProps} ->
                    case proplists:get_value(orgid, TeamProps) of
                        OrgID ->
                            State1 = State0#st{
                                team_props=TeamProps,
                                org_id=OrgID
                            },
                            {ok, Req1, State1};
                        _ ->
                            {ok, Req1, State0}
                    end
            end
    end.


allowed_methods(Req, State) ->
    {[<<"GET">>, <<"DELETE">>, <<"PATCH">>], Req, State}.

is_authorized(Req, State) ->
    osc_http:is_authorized(
        Req,
        fun(UserID) -> State#st{user_id=UserID} end,
        fun() -> State end
    ).

forbidden(Req0, State) ->
    #st{
        user_id=UserID,
        org_id=OrgID,
        team_props=TeamProps
    } = State,
    case OrgID =:= undefined orelse TeamProps =:= undefined of
        true ->
            {false, Req0, State};
        false ->
            {Method, Req1} = cowboy_req:method(Req0),
            IsOwner = osc_meta_org:is_owner(OrgID, UserID),
            Forbidden = case Method of
                <<"DELETE">> ->
                    %% The "owners" team cannot be deleted; only org owners can
                    %% delete teams within that org.
                    TeamName = proplists:get_value(name, TeamProps),
                    TeamName == <<"owners">> orelse not IsOwner;
                <<"PATCH">> ->
                    %% Only org owners can patch
                    not IsOwner;
                <<"GET">> ->
                    %% Only *team* members and org owners can view
                    IsMember = osc_meta_team:is_member(
                        proplists:get_value(id, TeamProps),
                        UserID
                    ),
                    not IsMember andalso not IsOwner
            end,
            {Forbidden, Req1, State}
    end.

resource_exists(Req, #st{team_props=undefined}=State) ->
    {false, Req, State};
resource_exists(Req, State) ->
    {true, Req, State}.

delete_resource(Req, #st{team_props=TeamProps}=State) ->
    TeamID = proplists:get_value(id, TeamProps),
    OrgID = proplists:get_value(orgid, TeamProps),
    ok = osc_meta_team:delete(OrgID, TeamID),
    {true, Req, State}.

content_types_accepted(Req, State) ->
    {
        [{{<<"application">>, <<"json-patch+json">>, '*'}, from_json_patch}],
        Req,
        State
    }.

from_json_patch(Req0, #st{team_props=TeamProps0}=State) ->
    {ok, Body, Req1} = cowboy_req:body(Req0),
    {ok, {Operation, Path, Value}} = osc_http:parse_json_patch(Body),
    case apply_patch(Operation, Path, Value, TeamProps0) of
        {ok, TeamProps1} ->
            {true, Req1, State#st{team_props=TeamProps1}};
        error ->
            {false, Req1, State}
    end.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, #st{team_props=TeamProps}=State) ->
    Members0 = proplists:get_value(members, TeamProps),
    Members1 = lists:map(
        fun({ID, Name}) ->
            {[{id, ID}, {name, Name}]}
        end,
        Members0
    ),
    Permissions0 = proplists:get_value(permissions, TeamProps),
    Permissions1 = lists:map(
        fun({ID, Perms}) ->
            {[{id, ID}, {permissions, {Perms}}]}
        end,
        Permissions0
    ),
    Body = {[
        {id, proplists:get_value(id, TeamProps)},
        {name, proplists:get_value(name, TeamProps)},
        {orgid, proplists:get_value(orgid, TeamProps)},
        {members, Members1},
        {permissions, Permissions1}
    ]},
    {jiffy:encode(Body), Req, State}.

apply_patch(<<"add">>, [<<"members">>], UserName, TeamProps) ->
    case osc_meta_user:lookup(UserName) of
        not_found ->
            %% This will 500 right now
            {error, unknown_user};
        {ok, UserProps} ->
            %% TODO: make sure this user isn't already on the team
            %% TODO: Do we automatically add the user to the org?
            %% I don't think there's a notion of membership outside
            %% team membership.
            ok = osc_meta_team:add_member(
                proplists:get_value(id, TeamProps),
                proplists:get_value(id, UserProps)
            ),
            NewMember = {
                proplists:get_value(id, UserProps),
                proplists:get_value(name, UserProps)
            },
            Members = [NewMember|proplists:get_value(members, TeamProps)],
            {ok, [{members, Members}|proplists:delete(members, TeamProps)]}
    end;
apply_patch(<<"remove">>, [<<"members">>], UserID, TeamProps) ->
    ok = osc_meta_team:remove_member(
        proplists:get_value(id, TeamProps),
        UserID
    ),
    Members = lists:filter(
        fun({ID, _Name}) -> ID =/= UserID end,
        proplists:get_value(members, TeamProps)
    ),
    {ok, [{members, Members}|proplists:delete(members, TeamProps)]};
apply_patch(<<"remove">>, [<<"permissions">>], PermID, TeamProps) ->
    ok = osc_meta_team:remove_permission(
        proplists:get_value(id, TeamProps),
        PermID
    ),
    Permissions = lists:filter(
        fun({ID, _Perms}) -> ID =/= PermID end,
        proplists:get_value(permissions, TeamProps)
    ),
    {ok, [{permissions, Permissions}|proplists:delete(permissions, TeamProps)]};
apply_patch(<<"add">>, [<<"permissions">>], {Permission}, TeamProps) ->
    {ok, PermID} = osc_meta_team:add_permission(
        proplists:get_value(id, TeamProps),
        Permission
    ),
    OldPerms = proplists:get_value(permissions, TeamProps),
    NewPerms = [{PermID, Permission}|OldPerms],
    {ok, [{permissions, NewPerms}|proplists:delete(permissions, TeamProps)]};
apply_patch(Op, Path, _, TeamProps) ->
    lager:error(
        "Got an unknown patch attempt: ~p, ~p for team ~p",
        [Op, Path, TeamProps]
    ),
    error.
