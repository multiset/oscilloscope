-module(osc_meta_user_handler).

-export([init/3, terminate/3]).
-export([
    rest_init/2,
    allowed_methods/2,
    is_authorized/2,
    forbidden/2,
    resource_exists/2,
    content_types_accepted/2,
    content_types_provided/2,
    from_json_patch/2,
    to_json/2
]).

-record(st, {
    user_id,
    user_props
}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
    ok.

rest_init(Req0, _) ->
    State = #st{},
    case cowboy_req:binding(user_id, Req0) of
        {undefined, Req1} ->
            {ok, Req1, State};
        {UserIDBin, Req1} ->
            UserID = list_to_integer(binary_to_list(UserIDBin)),
            case osc_meta_user:lookup(UserID) of
                not_found ->
                    {ok, Req1, State};
                {ok, UserProps} ->
                    {ok, Req1, State#st{user_props=UserProps}}
            end
    end.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PATCH">>], Req, State}.

is_authorized(Req, State) ->
    osc_http:is_authorized(
        Req,
        fun(UserID) -> State#st{user_id=UserID} end,
        fun() -> State end
    ).

forbidden(Req, State) ->
    #st{
        user_id=AuthUserID,
        user_props=UserProps
    } = State,
    Forbidden = case UserProps =/= undefined of
        false ->
            false;
        true ->
            proplists:get_value(id, UserProps) =/= AuthUserID
    end,
    {Forbidden, Req, State}.

resource_exists(Req, State) ->
    case State#st.user_props of
        undefined ->
            {false, Req, State};
        _ ->
            {true, Req, State}
    end.

content_types_accepted(Req, State) ->
    {
        [{{<<"application">>, <<"json-patch+json">>, '*'}, from_json_patch}],
        Req,
        State
    }.

from_json_patch(Req0, #st{user_props=UserProps0}=State) ->
    {ok, Body, Req1} = cowboy_req:body(Req0),
    {ok, {Operation, Path, Value}} = osc_http:parse_json_patch(Body),
    case apply_patch(Operation, Path, Value, UserProps0) of
        {ok, UserProps1} ->
            {true, Req1, State#st{user_props=UserProps1}};
        error ->
            {false, Req1, State}
    end.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, #st{user_props=UserProps}=State) ->
    Body = {[
        {id, proplists:get_value(id, UserProps)},
        {name, proplists:get_value(name, UserProps)},
        {emails, proplists:get_value(emails, UserProps)},
        {password, {[{last_modified, 0}]}} % TODO
    ]},
    {jiffy:encode(Body), Req, State}.

apply_patch(<<"add">>, [<<"emails">>], Email, UserProps) ->
    ok = osc_meta_user:add_email(proplists:get_value(id, UserProps), Email),
    Emails = [Email|proplists:get_value(emails, UserProps)],
    {ok, [{emails, Emails}|proplists:delete(emails, UserProps)]};
apply_patch(<<"remove">>, [<<"emails">>], Email, UserProps) ->
    ok = osc_meta_user:remove_email(proplists:get_value(id, UserProps), Email),
    Emails = lists:delete(Email, proplists:get_value(emails, UserProps)),
    {ok, [{emails, Emails}|proplists:delete(emails, UserProps)]};
apply_patch(<<"replace">>, [<<"password">>], {Passwords}, UserProps) ->
    %% TODO: verify that old password is valid
    ok = osc_meta_user:change_password(
        proplists:get_value(id, UserProps),
        proplists:get_value(<<"new">>, Passwords)
    ),
    NewProp = {password, {[{last_modified, osc_util:now()}]}},
    {ok, [NewProp|proplists:delete(password, UserProps)]};
apply_patch(Op, Path, _, UserProps) ->
    lager:error(
        "Got an unknown patch attempt: ~p, ~p for user ~p",
        [Op, Path, UserProps]
    ),
    error.
