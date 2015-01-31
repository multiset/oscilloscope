-module(osc_meta_stripe).

-export([create_customer/2, update_customer/2]).

-spec create_customer(Token, Description) -> Response when
    Token :: binary(),
    Description :: binary(),
    Response :: {ok, CustomerID} | {error, Error},
    CustomerID :: binary(),
    Error :: unknown_error.

create_customer(Token, Description) ->
    {Code, Body} = request(
        post,
        "/customers",
        <<"description=", Description/binary, "&card=", Token/binary>>
    ),
    case Code of
        200 ->
            {ok, proplists:get_value(<<"id">>, Body)};
        _ ->
            lager:warning(
                "Got bad response from Stripe on customer creation: ~p/~p",
                [Code, Body]
            ),
            {error, unknown_error}
    end.

-spec update_customer(CustomerID, Token) -> Response when
    CustomerID :: binary(),
    Token :: binary(),
    Response :: ok | {error, Error},
    Error :: unknown_error.

update_customer(CustomerID, Token) ->
    {Code, Body} = request(
        post,
        "/customers/" ++ binary_to_list(CustomerID),
        <<"card=", Token/binary>>
    ),
    case Code of
        200 ->
            ok;
        _ ->
            lager:warning(
                "Got bad response from Stripe on customer creation: ~p/~p",
                [Code, Body]
            ),
            {error, unknown_error}
    end.


-spec request(Method, Resource, ReqBody) -> {Code, RespBody} when
    Method :: post | put | get | delete,
    Resource :: string(),
    ReqBody :: binary(),
    Code :: pos_integer(),
    RespBody :: proplists:proplist().

request(Method, Resource, ReqBody) ->
    {ok, Key0} = application:get_env(osc_meta, stripe_key),
    Key1 = base64:encode(Key0),
    {ok, Code, _Headers, RespBody} = ibrowse:send_req(
        "https://api.stripe.com/v1" ++ Resource,
        [{"Authorization", <<"Basic ", Key1/binary>>}],
        Method,
        ReqBody
    ),
    {list_to_integer(Code), osc_util:unejsonify(jiffy:decode(RespBody))}.
