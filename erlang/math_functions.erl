-module('math_functions').
-export ([odd/1, even/1, filter/2, split/1, split1/1]).

even(X)->
    (X rem 2) == 0.

odd(X)->
    (X rem 2) == 1.


filter(F, L) ->
    [X || X <- L,
          F(X)].

split(L)->
    {filter(fun(X) -> odd(X) end, L), filter(fun(X) -> even(X) end, L)}.

split1(L) ->
    split_acc(L, [], []).

split_acc([H|T], Odds, Evens)->
    case (H rem 2) of
        0 -> split_acc(T, Odds, [H|Evens]);
        1 -> split_acc(T, [H|Odds], Evens)
    end;
split_acc([], Odds, Evens) ->
    {lists:reverse(Evens), lists:reverse(Odds)}.


            
