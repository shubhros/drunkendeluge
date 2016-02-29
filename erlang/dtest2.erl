-module(dtest2).
-export([f1/1, f2/1, f3/1,f4/1]).

f1({H, M, S})->
    (H+M*60)*60+S.

f2({H,M,S}) when is_integer(H) ->
    (H+M*60)*60 + S.

f3({H,M,S}) ->
    print(H,M,S),
    (H+M*60)*60 + S.

print(H,M,S) ->
    Str = integer_to_list(H) ++ ":" ++ integer_to_list(M) ++ ":" ++
        integer_to_list(S),
    io:format("~s", [Str]).

f4({H,M,S}) when is_float(H) ->
    print(H,M,S),
    (H+M*60)*60 + S.
