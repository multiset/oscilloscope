-module(osc_http_auth).

%%
%% Implementation of a secure cookie mechanism as described in [1].
%%
%% [1]: https://tools.ietf.org/html/draft-secure-cookie-session-protocol-02
%%

-export([
    outbound/2,
    inbound/1
]).

-define(BLOCK_KEY, <<46,21,216,240,106,79,10,155,109,41,49,251,168,118,3,149, 18,43,148,111,140,55,37,177,61,222,194,40,88,36,182,71>>).
-define(MAC_KEY, <<46,21,216,240,106,79,10,155,109,41,49,251,168,118,3,149, 18,43,148,111,140,55,37,177,61,222,194,40,88,36,182,72>>).

outbound(Data, Lifetime) ->
    IV = crypto:strong_rand_bytes(16),
    ETime = osc_util:now() + Lifetime,
    {TID, Block, Mac} = ciphers(),
    %% TODO
    %% {ok, BlockKey} = application:get_env(osc_http, Block),
    %% {ok, HMACKey} = application:get_env(osc_http, Mac),
    BlockKey = ?BLOCK_KEY,
    MACKey = ?MAC_KEY,
    Packed = pack({Data, Lifetime}),
    Encrypted = crypto:block_encrypt(Block, BlockKey, IV, Packed),
    Tag = crypto:hmac(
        Mac,
        MACKey,
        <<Encrypted/binary, ETime:64/integer, TID/integer, IV/binary>>
    ),
    Encoded = base64:encode(term_to_binary(
        {Encrypted, ETime, TID, IV, Tag},
        [compressed]
    )),
    {ok, Encoded}.

inbound(Cookie) ->
    {Encrypted, ETime, TID, IV, Tag0} = binary_to_term(base64:decode(Cookie)),
    {Block, Mac} = ciphers(TID),
    %% TODO
    %% {ok, BlockKey} = application:get_env(osc_http, Block),
    %% {ok, HMACKey} = application:get_env(osc_http, Mac),
    BlockKey = ?BLOCK_KEY,
    MACKey = ?MAC_KEY,
    Tag1 = crypto:hmac(
        Mac,
        MACKey,
        <<Encrypted/binary, ETime:64/integer, TID/integer, IV/binary>>
    ),
    case {Tag0 == Tag1, osc_util:now() < ETime} of
        {false, _} ->
            {error, mac};
        {true, false} ->
            {error, expired};
        {true, true} ->
            Binary = crypto:block_decrypt(Block, BlockKey, IV, Encrypted),
            {Data, Lifetime} = unpack(Binary),
            {ok, Data, Lifetime}
    end.

pack(Data) ->
    Unpadded = term_to_binary(Data, [compressed]),
    PadSize = 16 - ((byte_size(Unpadded) + 1) rem 16),
    Pad = binary:copy(<<0>>, PadSize),
    <<PadSize/integer, Pad/binary, Unpadded/binary>>.

unpack(Packed0) ->
    <<PadSize/integer, Packed1/binary>> = Packed0,
    <<_Pad:PadSize/binary, Data/binary>> = Packed1,
    binary_to_term(Data).

ciphers() ->
    {0, aes_cbc256, sha}.

ciphers(0) ->
    {aes_cbc256, sha}.
