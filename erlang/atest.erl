-module(atest).
-compile(export_all).

start(Tag) ->
    spawn(fun() -> loop(Tag) end).

loop(Tag) ->
    sleep(),
    Val = btest:x(),
    io:format("Vsn3 (~p) v:x() = ~p~n", [Tag, Val]),
    loop(Tag).

sleep() ->
    receive
        after 3000 -> true
        end.
                  
