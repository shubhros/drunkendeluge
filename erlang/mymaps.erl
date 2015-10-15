-module('mymaps').
-export([count_characters/1]).

count_characters(Str)->
    count_characters(Str, #{}).

count_characters([H|T], X) ->
    case maps:is_key(H, X) of
        false-> count_characters(T, maps:put(H,1,X));
        true -> Count = maps:get(H, X),
                count_characters(T, maps:update(H, Count+1, X))
    end;

count_characters([], X) ->
    X.
