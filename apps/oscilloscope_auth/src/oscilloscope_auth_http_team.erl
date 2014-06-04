-module(oscilloscope_auth_http_team).

-include("oscilloscope_auth.hrl").

-export([
    init/1,
    is_authorized/2,
    malformed_request/2,
    allowed_methods/2,
    content_types_provided/2,
    to_json/2
]).

-record(state, {
    org_id,
    ids,
    names
}).

init([]) ->
    {ok, #state{}}.

allowed_methods(ReqData, State) ->
    {['PUT', 'DELETE'], ReqData, State}.

malformed_request(ReqData, State0) ->
    {IsMalformed, State} = case {wrq:req_body(ReqData), wrq:method(ReqData)} of
        {undefined, 'DELETE'} ->
            % DELETE is for removing users from team. Must supply user ids
            {State0, true};
        {undefined, 'PUT'} ->
            % Create team request
            try list_to_integer(wrq:path_info(team_name_or_id, ReqData)) of
                _ -> {true, State0}
            catch error:badarg ->
                {false, State0}
            end;
        {Body, 'DELETE'} ->
            % Add or remove users from team
            case parse_and_validate_body(Body) of
                false ->
                    {true, State0};
                {IDs, Names} ->
                    State1 = State0#state{
                        ids=IDs,
                        names=Names
                    },
                    {false, State1}
            end
    end,
    {IsMalformed, ReqData, State}.

% Parses PUT body into a list of user IDs and list of user names. If body is
% invalid, returns false.
-spec parse_and_validate_body(binary()) -> {[integer()], [binary()]} | false.
parse_and_validate_body(Body) ->
    case oscilloscope_auth_util:parse_body(Body) of
        false ->
            false;
        ParsedBody ->
            parse_and_validate_body(ParsedBody, [], [])
    end.

parse_and_validate_body([], UserIDsAcc, UserNamesAcc) ->
    {UserIDsAcc, UserNamesAcc};
parse_and_validate_body([Pair|Pairs], UserIDsAcc, UserNamesAcc) ->
    {Key, Value} = Pair,
    if Key == <<"userid">> ->
        try binary_to_integer(Value) of
            UserID ->
                parse_and_validate_body(Pairs, [UserID|UserIDsAcc], UserNamesAcc)
        catch error:badarg ->
            false
        end;
    Key == <<"username">> ->
        try binary_to_integer(Value) of
            _ ->
                false % username can't be integer
        catch error:badarg ->
            parse_and_validate_body(Pairs, UserIDsAcc, [Value|UserNamesAcc])
        end;
    true ->
        false
    end.

is_authorized(ReqData, State) ->
    NameOrID = wrq:path_info(org_name_or_id, ReqData),
    OrgID = try list_to_integer(NameOrID) of
        OrgID0 ->
            OrgID0
    catch error:badarg ->
        oscilloscope_auth_org:get_id(NameOrID)
    end,

    IsAuthed = case oscilloscope_auth_util:get_authorized_user(ReqData) of
        #user{}=User ->
            IsOwner = oscilloscope_auth_team:is_owner(User, OrgID),
            case {IsOwner, wrq:method(ReqData)} of
                {true, _} ->
                    true;
                {false, 'DELETE'} ->
                    % Check if user is removing self from team
                    DeletesName = lists:all(
                        fun(Name) -> Name == User#user.name end,
                        State#state.names
                    ),
                    DeletesID = lists:all(
                        fun(ID) -> ID == User#user.id end,
                        State#state.ids
                    ),
                    IdsNotEmpty = length(State#state.ids) > 0,
                    NamesNotEmpty = length(State#state.names) > 0,
                    if DeletesName, DeletesID, IdsNotEmpty orelse NamesNotEmpty ->
                        true;
                    true ->
                        false
                    end;
                {false, 'PUT'} ->
                    false
            end;
        _Unauthorized ->
            false
    end,
    Ret = if IsAuthed -> true; true -> "Basic realm=oscilloscope" end,
    {Ret, ReqData, State#state{org_id=OrgID}}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    % TODO: Handle errors, eg. team name already exists
    TeamName = wrq:path_info(team_name_or_id, ReqData),
    case {wrq:method(ReqData), State} of
        {'PUT', #state{ids=[], names=[]}} ->
            % create team
            {ok, _} = oscilloscope_auth_team:create(State#state.org_id, TeamName);
        {'PUT', #state{names=NamesToAdd, ids=IdsToAdd}} ->
            % add users to team
            ok = oscilloscope_auth_team:add_members(
                State#state.org_id,
                TeamName,
                lists:flatten([NamesToAdd, IdsToAdd])
            );
        {'DELETE', #state{ids=[], names=[]}} ->
            % delete team
            {ok, _} = oscilloscope_auth_team:delete(State#state.org_id, TeamName);
        {'DELETE', #state{names=Names, ids=Ids}} ->
            % remove users from team
            ok = oscilloscope_auth_team:remove_members(
                State#state.org_id,
                TeamName,
                Names ++ Ids
            )
    end,

    {<<"{\"ok\": true}">>, ReqData, State}.
