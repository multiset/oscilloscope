-module(oscilloscope_http_user).

-export([
    init/1,
    ping/2,
    process_post/2,
    malformed_request/2,
    allowed_methods/2
]).

-export([
    create_user/4
]).

-record(st, {
    new_user,
    ports
}).

-include("oscilloscope_http.hrl").

init([]) ->
    Ports = get_unused_ports(),
    {ok, _, Users} = oscilloscope_sql:named(all_users, []),
    lists:foldl(fun({Username, Password, Port, Id}, _) ->
        User = #user{
            id=Id,
            name=Username,
            password=Password,
            port=Port
        },
        ets:insert(user_cache, User)
    end, nil, Users),
    {ok, #st{ports=Ports}}.

get_unused_ports() ->
    {ok, _, UsedPorts0} = oscilloscope_sql:named(used_ports, []),
    UsedPorts = lists:map(fun({X}) -> X end, UsedPorts0),
    AllPorts = sets:from_list(lists:seq(1025, 65535)),
    sets:to_list(sets:subtract(AllPorts, sets:from_list(UsedPorts))).

ping(Req, #st{}=State) ->
    {pong, Req, State}.

allowed_methods(Req, #st{}=State) ->
    {['POST'], Req, State}.

malformed_request(Req, #st{}=State) ->
    Props = oscilloscope_http_util:split_post_body(Req),
    case get_args(["username", "email", "password"], Props) of
        {ok, [Username, Email, Password]} ->
            {false, Req, State#st{new_user={Username, Email, Password}}};
        false ->
            {true, Req, State}
    end.

process_post(Req, State) ->
    #st{new_user={Username, Email, Password}, ports=[Port|Ports]} = State,
    create_user(Username, Port, Email, Password),
    Resp = wrq:set_resp_body(jiffy:encode({[{ok, true}, {port, Port}]}), Req),
    {true, Resp, State#st{ports=Ports, new_user=undefined}}.


create_user(Username, Port, Email, Password) ->
    {ok, Salt} = bcrypt:gen_salt(4),
    {ok, Hash} = bcrypt:hashpw(Password, Salt),
    {ok, _} = oscilloscope_sql:named(
        insert_user,
        [list_to_binary(Username), Port, Email, list_to_binary(Hash)]
    ),
    {ok, _, [{Id}]} = oscilloscope_sql:named(select_user_id, [Username]),
    User = #user{
        id=Id,
        name=list_to_binary(Username),
        password=list_to_binary(Hash),
        port=Port
    },
    ets:insert(user_cache, User).


get_args(Key, Props) ->
    get_args(Key, Props, []).

get_args([], _Props, Acc) ->
    {ok, lists:reverse(Acc)};
get_args([Key|Keys], Props, Acc) ->
    case lists:keyfind(Key, 1, Props) of
        {Key, Value} ->
            get_args(Keys, Props, [Value|Acc]);
        false ->
            false
    end.
