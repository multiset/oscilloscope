% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.
%
% Original work by Robert Newson/Apache CouchDB project. Heavily modified by
% Multiset, Inc., for sha512 support and coding style.

-module(osc_meta_passwords).

-export([hash/1, verify/2]).
-export([pbkdf2/3, pbkdf2/4]).

-define(MAX_DERIVED_KEY_LENGTH, (1 bsl 32 - 1)).
-define(SHA512_OUTPUT_LENGTH, 64).

-spec hash(Password) -> {ok, Hashed} when
    Password :: binary(),
    Hashed :: binary().

hash(Password) ->
    {ok, Iterations} = application:get_env(osc_meta, pbkdf2_iterations),
    Salt = crypto:rand_bytes(16),
    pbkdf2(Password, Salt, Iterations).


-spec verify(Password, Hash) -> Verified when
    Password :: binary(),
    Hash :: binary(),
    Verified :: boolean().

verify(Password, <<0:8/integer, HashParts/binary>>=Hash0) ->
    <<
        Iterations:16/integer,
        SaltSize:8/integer,
        Salt:SaltSize/binary,
        _Rest/binary
    >> = HashParts,
    {ok, Hash1} = pbkdf2(Password, Salt, Iterations),
    eq(Hash0, Hash1);
verify(_, <<Scheme:8/integer, _/binary>>) ->
    %% Hashing scheme is unknown
    lager:error("osc_meta_passwords found unknown hashing scheme: ~p", Scheme),
    false.

pbkdf2(Password, Salt, Iterations) ->
    pbkdf2(Password, Salt, Iterations, ?SHA512_OUTPUT_LENGTH).

-spec pbkdf2(Password, Salt, Iterations, Length) -> {ok, Hash} | {error, E} when
    Password :: binary(),
    Salt :: binary(),
    Iterations :: pos_integer(),
    Length :: pos_integer(),
    Hash :: binary(),
    E :: derived_key_too_long.

pbkdf2(_, _, _, DerivedLength) when DerivedLength > ?MAX_DERIVED_KEY_LENGTH ->
    {error, derived_key_too_long};
pbkdf2(Password, Salt, Iterations, DerivedLength) ->
    L = ceiling(DerivedLength / ?SHA512_OUTPUT_LENGTH),
    <<Hash:DerivedLength/binary, _/binary>> = iolist_to_binary(pbkdf2_int(
        Password,
        Salt,
        Iterations,
        L,
        1,
        []
    )),

    SaltSize = byte_size(Salt),
    Out = <<
        0:8/integer,
        Iterations:16/integer,
        SaltSize:8/integer,
        Salt/binary,
        DerivedLength:8/integer,
        Hash/binary
    >>,
    {ok, Out}.


-spec pbkdf2_int(Password, Salt, Iterations, BCount, BIndex, Acc) -> Acc when
    Password :: binary(),
    Salt :: binary(),
    Iterations :: pos_integer(),
    BCount :: pos_integer(),
    BIndex :: pos_integer(),
    Acc :: iolist().

pbkdf2_int(_, _, _, BlockCount, BlockIndex, Acc) when BlockIndex > BlockCount ->
    lists:reverse(Acc);
pbkdf2_int(Password, Salt, Iterations, BlockCount, BlockIndex, Acc) ->
    Block = compute_block(
        Password,
        Salt,
        Iterations,
        BlockIndex,
        1,
        <<>>,
        <<>>
    ),
    pbkdf2_int(
        Password,
        Salt,
        Iterations,
        BlockCount,
        BlockIndex + 1,
        [Block|Acc]
    ).


-spec compute_block(Password, Salt, Is, BIndex, I, Prev, Acc) -> Block when
    Password :: binary(),
    Salt :: binary(),
    Is :: pos_integer(),
    BIndex :: pos_integer(),
    I :: pos_integer(),
    Prev :: binary(),
    Acc :: binary(),
    Block :: binary().

compute_block(_, _, Iterations, _, Iteration, _, Acc) when Iteration > Iterations ->
    Acc;
compute_block(Password, Salt, Iterations, BlockIndex, 1, _Prev, _Acc) ->
    InitialBlock = crypto:hmac(
        sha512,
        Password,
        <<Salt/binary,BlockIndex:32/integer>>
    ),
    compute_block(
        Password,
        Salt,
        Iterations,
        BlockIndex,
        2,
        InitialBlock,
        InitialBlock
    );
compute_block(Password, Salt, Iterations, BlockIndex, Iteration, Prev, Acc) ->
    Next = crypto:hmac(sha512, Password, Prev),
    compute_block(
        Password,
        Salt,
        Iterations,
        BlockIndex,
        Iteration + 1,
        Next,
        crypto:exor(Next, Acc)
    ).


%% check for equality without short-circuits to avoid timing attacks.
-spec eq(binary(), binary()) -> boolean().
eq(X, Y) when bit_size(X) =/= bit_size(Y) ->
    false;
eq(X, Y) ->
    eq(X, Y, 0).

eq(<<>>, <<>>, Result) ->
    Result == 0;
eq(<<X:8/integer, Xs/binary>>, <<Y:8/integer, Ys/binary>>, Result) ->
    eq(Xs, Ys, (X bxor Y) bor Result).


-spec ceiling(number()) -> integer().
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.
