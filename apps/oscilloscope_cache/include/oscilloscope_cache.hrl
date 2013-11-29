%% Minimim amount of seconds to wait before persisting to disk
-define(MIN_PERSIST_AGE, 300).

%% TODO: try zlib:zip instead
-define(VALENCODE(V), zlib:compress(term_to_binary(V))).
-define(VALDECODE(V), binary_to_term(zlib:uncompress(V))).

%% Number of bytes to store in each value
-define(MIN_CHUNK_SIZE, 1000).
-define(MAX_CHUNK_SIZE, 1024).

%% Dynamo/commutator things. Should go in config
-define(DYNAMO_TABLE, <<"oscilloscope-test">>).
-define(DYNAMO_SCHEMA, [{<<"id">>, n}, {<<"t">>, n}, {<<"v">>, b}]).
-define(DYNAMO_REGION, "us-east-1").
-define(DYNAMO_ACCESSKEY, "AKIAJNCK2CQXCEEM6MDA").
-define(DYNAMO_SECRETKEY, "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").
