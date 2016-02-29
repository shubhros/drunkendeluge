#/usr/bin/env escript

fac(0) ->
    1;
fac(N) ->
    N * fac(N - 1).

main([A]) ->
    I = list_to_integer(atom_to_list(A)),
    F = fac(I),
    io:format("factorial ~w = ~w~n", [I, F]),
    init:stop().
