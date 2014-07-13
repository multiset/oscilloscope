-module(oscilloscope_auth_http_create_team).

-include("oscilloscope_auth.hrl").

-export([
    init/1,
    ping/2,
    resource_exists/2,
    is_conflict/2,
    is_authorized/2,
    allow_missing_post/2,
    is_malformed/2,
    allowed_methods/2,
    content_types_accepted/2,
    from_json/2
]).

-record(state, {
    team_name,
    org,
    team,
    user
}).

init([]) ->
    {ok, #state{}}.

ping(ReqData, State) ->
    {pong, ReqData, State}.

allowed_methods(ReqData, State) ->
    {['POST'], ReqData, State}.

allow_missing_post(ReqData, State) ->
    {false, ReqData, State}.

resource_exists(ReqData, State) ->
    OrgName = wrq:path_info(org_name, ReqData),

    case oscilloscope_auth_org:lookup(OrgName) of
        {ok, Org} ->
            {true, ReqData, State#state{org=Org}};
        not_found ->
            {false, ReqData, State}
    end.

is_malformed(ReqData, State) ->
    case wrq:req_body(ReqData) of
        undefined ->
            {true, ReqData, State};
        Body ->
            try jiffy:decode(Body) of
                {KVs} when is_list(KVs) ->
                    case lists:keyfind(<<"team_name">>, 1, KVs) of
                        {<<"team_name">>, Name} ->
                            case oscilloscope_auth_team:lookup(Org, Name) of
                                {ok, Team} ->
                                    {false, ReqData, State#state{team=Team}};
                                not_found ->
                                    {true, ReqData, State}
                            end;
                        _ ->
                            {true, ReqData, State}
                    end;
                _ ->
                    {true, ReqData, State}
            catch throw:_ ->
                {true, ReqData, State}
            end
    end.

is_authorized(ReqData, State) ->
    #state{org=Org, user=ReqUser} = State,

    IsAuthed = case oscilloscope_auth_util:get_authorized_user(ReqData) of
        #user{}=User ->
            IsOwner = oscilloscope_auth_org:is_owner(Org, User),
            if IsOwner ->
                true;
            true ->
                case ReqUser of
                    undefined ->
                        % If create/delete team-type request
                        {false, false};
                    _ ->
                        % If add/remove user to/from team req
                        ReqUser#user.name =:= User#user.name
                end
            end;
        _ ->
            false
    end,
    Ret = if IsAuthed -> true; true -> "Basic realm=oscilloscope" end,
    {Ret, ReqData, State}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", from_json}], ReqData, State}.

from_json(ReqData, State) ->
    % TODO: Handle errors, eg. team name already exists
    #state{org=Org, team=Team, user=User} = State,
    case {wrq:method(ReqData), wrq:path_info(user_name, ReqData)} of
        {'POST', undefined} ->
            % create team
            TeamName = wrq:path_info(team_name, ReqData),
            {ok, NewTeam} = oscilloscope_auth_team:create(Org, TeamName),
            ok = oscilloscope_auth_team:add_member(Org, NewTeam, User);
        {'DELETE', undefined} ->
            % delete team
            ok = oscilloscope_auth_team:delete(Org, Team);
        {'POST', _} ->
            % add user to team
            ok = oscilloscope_auth_team:add_member(Org, Team, User);
        {'DELETE', _} ->
            ok = oscilloscope_auth_team:remove_member(Org, Team, User)
    end,
    {true, wrq:set_resp_body(<<"{\"ok\": true}">>, ReqData), State}.
