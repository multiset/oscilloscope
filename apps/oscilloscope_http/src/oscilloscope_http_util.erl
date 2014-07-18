-module(oscilloscope_http_util).


-export([
    get_path_info/2,
    get_header/2,
    get_qs/2,
    split_post_body/1
]).


get_path_info(Key, Req) ->
    case wrq:path_info(Key, Req) of
        undefined -> throw(badpath);
        Value -> Value
    end.


get_header(Key, Req) ->
    case wrq:get_req_header(Key, Req) of
        undefined -> throw(badheader);
        Value -> Value
    end.


get_qs(Key, Req) ->
    case wrq:get_qs_value(Key, Req) of
        undefined -> throw(badqs);
        Value -> Value
    end.


split_post_body(Req) ->
    Body = wrq:req_body(Req),
    Pairs = string:tokens(binary_to_list(Body), "&"),
    [list_to_tuple(string:tokens(Pair, "=")) || Pair <- Pairs].
