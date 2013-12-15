%% TODO: try zlib:zip instead
-define(VALENCODE(V), zlib:compress(term_to_binary(V))).
-define(VALDECODE(V), binary_to_term(zlib:uncompress(V))).

