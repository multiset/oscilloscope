-module(osc_meta_team_handler).

%% TODO: This is still for users!

-export([init/3, terminate/3]).
-export([
    rest_init/2,
    allowed_methods/2,
    resource_exists/2,
    delete_resource/2,
    content_types_accepted/2,
    content_types_provided/2,
    from_json_patch/2,
    to_json/2
]).

-record(st, {
    team_props
}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
    ok.

rest_init(Req, _State) ->
    {ok, Req, #st{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"DELETE">>, <<"PATCH">>], Req, State}.

resource_exists(Req0, State) ->
    {Bindings, Req1} = cowboy_req:bindings(Req0),
    IDs = {
        proplists:get_value(orgid, Bindings),
        proplists:get_value(teamid, Bindings)
    },
    case IDs of
        {undefined, _} ->
            {false, Req1, State};
        {_, undefined} ->
            {false, Req1, State};
        {OrgIDBin, TeamIDBin} ->
            OrgID = list_to_integer(binary_to_list(OrgIDBin)),
            TeamID = list_to_integer(binary_to_list(TeamIDBin)),
            case osc_meta_team:lookup(TeamID) of
                not_found ->
                    {false, Req1, State};
                {ok, TeamProps} ->
                    case proplists:get_value(orgid, TeamProps) of
                        OrgID ->
                            {true, Req1, State#st{team_props=TeamProps}};
                        _ ->
                            {false, Req1, State}
                    end
            end
    end.

delete_resource(Req, #st{team_props=TeamProps}=State) ->
    TeamID = proplists:get_value(id, TeamProps),
    OrgID = proplists:get_value(orgid, TeamProps),
    case proplists:get_value(name, TeamProps) of
        <<"owners">> ->
            lager:notice(
                "User attempted to DELETE owner team from org ~p", [OrgID]
            ),
            {false, Req, State};
        _ ->
            ok = osc_meta_team:delete(OrgID, TeamID),
            {true, Req, State}
    end.

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
    Body = {[
        {id, proplists:get_value(id, TeamProps)},
        {name, proplists:get_value(name, TeamProps)},
        {orgid, proplists:get_value(orgid, TeamProps)},
        {members, Members1}
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
                proplists:get_value(orgid, TeamProps),
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
        proplists:get_value(orgid, TeamProps),
        proplists:get_value(id, TeamProps),
        UserID
    ),
    Members = lists:filter(
        fun({ID, _Name}) -> ID =/= UserID end,
        proplists:get_value(members, TeamProps)
    ),
    {ok, [{members, Members}|proplists:delete(members, TeamProps)]};
apply_patch(Op, Path, _, TeamProps) ->
    lager:error(
        "Got an unknown patch attempt: ~p, ~p for team ~p",
        [Op, Path, TeamProps]
    ),
    error.
