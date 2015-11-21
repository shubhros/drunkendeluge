-module(lib_misc).
-export([for/3, qsort/1, pythag/1, perms/1, max/2, f/1, filter/2, odds_and_evens/1,
       tuple_to_list/1, my_time_func/1, sum/1,mysum/1, square/1]).

-author({shubhro, sinha}).

for(Max, Max, F)->
    [F(Max)];
for(I, Max, F) ->
    [F(I) | for(I+1, Max, F)].

qsort([])->
    [];
qsort([Pivot|T])->
    qsort([X || X <- T,
                X < Pivot])
    ++ [Pivot] ++
        qsort([X || X <- T,
                    X >= Pivot]).

pythag(N) ->
    [ {A, B, C} ||
        A <- lists:seq(1, N),
        B <- lists:seq(1, N),
        C <- lists:seq(1, N),
        
        A + B + C =< N,
        A*A + B*B =:= C*C
    ].


perms([]) ->
    [[]];
    
perms(L) ->
    [[H|T] || H <- L,
               T <- perms(L -- [H])].

max(X, Y) when X > Y ->
    X;
max(X, Y) ->
    Y.

f(X) when (X == 0) or (1/X > 2) ->
    10;
f(X) ->
    0.

filter(P, [H|T]) ->
    case P(H) of
        true -> [H | filter(P, T)];
        false -> filter(P, T)
    end;
filter(P, []) ->
    [].


odds_and_evens(L) ->
    odds_and_evens_acc(L, [], []).

odds_and_evens_acc([H|T], Odds, Evens)->
    case (H rem 2) of
        0 -> odds_and_evens_acc(T, Odds, [H|Evens]);
        1 -> odds_and_evens_acc(T, [H|Odds], Evens)
    end;
odds_and_evens_acc([], Odds, Evens) ->
    {lists:reverse(Odds), lists:reverse(Evens)}.

tuple_to_list(T)->
    [element(X, T) || X <- lists:seq(1, tuple_size(T))].

my_time_func(F)->
    {S1, S2, S3} = now(),
    F(),
    {S4, S5, S6} = now(),
    {S4 - S1, S5 - S2, S6 - S3}.
    

sum(L) ->
    sum(L, 0).

sum([], N) ->
    N;
sum([H|T], N) ->
    sum(T, H+N).

mysum(L)->
   lists:sum(L).

square(X) ->
    X * X.
    
