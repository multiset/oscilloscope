-module(oscilloscope_auth_http_team).

-include("oscilloscope_auth.hrl").

-export([team_exists/1]).

-export([
    init/1,
    ping/2,
    resource_exists/2,
    is_conflict/2,
    is_authorized/2,
    is_malformed/2,
    allowed_methods/2,
    content_types_accepted/2,
    from_json/2
]).

-record(state, {
    org,
    team,
    user
}).

init([]) ->
    {ok, #state{}}.

ping(ReqData, State) ->
    {pong, ReqData, State}.

allowed_methods(ReqData, State) ->
    {['PUT', 'DELETE'], ReqData, State}.

team_exists(ReqData) ->
    OrgName = wrq:path_info(org_name, ReqData),
    TeamName = wrq:path_info(team_name, ReqData),
    case oscilloscope_auth_org:lookup(OrgName) of
        {ok, Org} ->
            case wrq:method(ReqData) of
                'DELETE' ->
                    oscilloscope_auth_team:lookup(Org, TeamName) =/= not_found;
                'PUT' ->
                    true
            end;
        not_found ->
            false
    end.

resource_exists(ReqData, State) ->
    OrgName = wrq:path_info(org_name, ReqData),
    TeamName = wrq:path_info(team_name, ReqData),

    {Exists, Org, Team, User} = case oscilloscope_auth_org:lookup(OrgName) of
        {ok, Org0} ->
            TeamLookup = oscilloscope_auth_team:lookup(Org0, TeamName),
            {TeamExists, Team1} = case TeamLookup of
                {ok, Team0} ->
                    {true, Team0};
                not_found ->
                    {false, undefined}
            end,
            case wrq:method(ReqData) of
                'DELETE' ->
                    case wrq:path_info(user_name, ReqData) of
                        undefined ->
                            % delete team req
                            {TeamExists, Org0, Team1, undefined};
                        LUserName ->
                            % remove member req
                            UserName = list_to_binary(LUserName),
                            case oscilloscope_auth_user:lookup(UserName) of
                                {ok, User0} ->
                                    IsMember = oscilloscope_auth_team:is_member(
                                        Org0, Team1, User0
                                    ),
                                    {TeamExists andalso IsMember, Org0, Team1, User0};
                                not_found ->
                                    {false, undefined, undefined, undefined}
                            end
                    end;
                'PUT' ->
                    case wrq:path_info(user_name, ReqData) of
                        undefined ->
                            % create team req
                            {true, Org0, Team1, undefined};
                        LUserName ->
                            % add user to team req
                            UserName = list_to_binary(LUserName),
                            case oscilloscope_auth_user:lookup(UserName) of
                                {ok, User0} ->
                                    {TeamExists, Org0, Team1, User0};
                                not_found ->
                                    {false, undefined, undefined, undefined}
                            end
                    end
            end;
        not_found ->
            {false, undefined, undefined, undefined}
    end,
    {Exists, ReqData, State#state{org=Org, team=Team, user=User}}.


is_malformed(ReqData, State) ->
    #state{team=Team, user=User} = State,
    {not oscilloscope_auth_team:is_member(Team, User), ReqData, State}.

is_conflict(ReqData, State) ->
    #state{org=Org, team=Team, user=User} = State,
    Conflict = case wrq:path_info(user_name, ReqData) of
        undefined ->
            % team must not already exist to be created
            TeamName = wrq:path_info(team_name, ReqData),
            oscilloscope_auth_team:lookup(Org, TeamName) =/= not_found;
        _ ->
            % user must not already be member of team
            oscilloscope_auth_team:is_member(Org, Team, User)
    end,
    {Conflict, ReqData, State}.

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
        {'PUT', undefined} ->
            % create team
            TeamName = wrq:path_info(team_name, ReqData),
            {ok, NewTeam} = oscilloscope_auth_team:create(Org, TeamName),
            ok = oscilloscope_auth_team:add_member(Org, NewTeam, User);
        {'DELETE', undefined} ->
            % delete team
            ok = oscilloscope_auth_team:delete(Org, Team);
        {'PUT', _} ->
            % add user to team
            ok = oscilloscope_auth_team:add_member(Org, Team, User);
        {'DELETE', _} ->
            ok = oscilloscope_auth_team:remove_member(Org, Team, User)
    end,
    {true, wrq:set_resp_body(<<"{\"ok\": true}">>, ReqData), State}.
