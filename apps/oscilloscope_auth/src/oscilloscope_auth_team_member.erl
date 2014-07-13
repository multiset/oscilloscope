-module(oscilloscope_auth_http_team_member).

-include("oscilloscope_auth.hrl").

-export([user_exists/1]).

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

% For request DELETE/PUT /orgs/:org/teams/:team/members/:member, this guard
% causes the request 404 if:
% 1) :org doesn't exist, or
% 2) :team doesn't exist, or
% 3) :member isn't member of :team when DELETE req, or
% 3) :member isn't member of :org when PUT req
user_exists(ReqData) ->
    OrgName = wrq:path_info(org_name, ReqData),
    TeamName = wrq:path_info(team_name, ReqData),
    UserName = wrq:path_info(user_name, ReqData),
    case oscilloscope_auth_org:lookup(OrgName) of
        {ok, Org} ->
            case oscilloscope_auth_team:lookup(Org, TeamName) of
                not_found ->
                    false;
                {ok, Team} ->
                    case oscilloscope_auth_user:lookup(UserName) of
                        not_found ->
                            false;
                        {ok, User} ->
                            case wrq:method(ReqData) of
                                'PUT' ->
                                    oscilloscope_auth_org:is_member(Org, User);
                                'DELETE' ->
                                    oscilloscope_auth_team:is_member(
                                        Org,
                                        Team,
                                        User
                                    )
                            end
                    end
            end;
        not_found ->
            false
    end.

init([]) ->
    {ok, #state{}}.

ping(ReqData, State) ->
    {pong, ReqData, State}.

allowed_methods(ReqData, State) ->
    {['PUT', 'DELETE'], ReqData, State}.

resource_exists(ReqData, State) ->
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

is_authorized(ReqData, State) ->
    OrgName = wrq:path_info(org_name, ReqData),
    TeamName = wrq:path_info(team_name, ReqData),
    UserName = wrq:path_info(user_name, ReqData),
    {ok, Org} = oscilloscope_auth_org:lookup(OrgName),
    {ok, Team} = oscilloscope_auth_team:lookup(TeamName),
    {ok, ReqUser} = oscilloscope_auth_team:lookup(UserName),

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
    {Ret, ReqData, State#state{org=Org, team=Team, user=User}}.

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
