-module(my_list).
-export([sum/1, map/2, filter/2]).

sum([H|T]) ->
    H + sum(T);
sum([]) ->
    0.

map(_, [])->
    [];
map(F, [H|T]) ->
    [F(H) | map(F, T)].

filter(P, L)->
    [X || X <- L,
          P(X)];
filter(P,[]) ->
    [].

             

