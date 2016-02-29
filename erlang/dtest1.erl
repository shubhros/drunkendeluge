-module(dtest1).
-export([f1/0, f2/0, test/0, actorial/1]).

f1() ->
    X = erlang:time(),
    seconds(X).

seconds({_Year, _Month, _Day, Hour, Min, Sec})->
    (Hour * 60 + Min) * 60 + Sec.

f2() ->
    tuple_size(list_to_tuple({'a', 'b', 'c'})).

test() ->
    actorial(-5).

actorial(0) ->
    1;
actorial(N) ->
    N * actorial(N - 1).


    
