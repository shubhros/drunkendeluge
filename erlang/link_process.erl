-module(link_process).
-compile(export_all).


on_exit(Pid, Fun) ->
    spawn(fun() ->
                  Ref = monitor(process, Pid),
                  receive
                      {'DOWN', Ref, process, Pid, Why} ->
                          Fun(Pid, Why)
                  end
          end).

ltoa() ->
    receive
        X ->
            io:format("~w~n", [list_to_atom(X)]),
            ltoa()
    end.

process_why(_Pid, Why) ->
    io:format("process died with ~w~n", [Why]).


keep_alive(Name, Fun) ->
    register(Name, Pid = spawn(Fun)),
    on_exit(Pid, fun(_Why) ->
                         keep_alive(Name, Fun) end).
                 
detail_why(Pid, Why) ->
    {_, Time} = statistics(runtime),
    io:format("~p died of ~w stayed up for ~w seconds ~n", [Pid, Why, Time/1000]).

myspawn(Mod, Func, Args) ->
    Pid = spawn(Mod, Func, Args),
    statistics(runtime),
    on_exit(Pid, fun detail_why/2),
    Pid.


my_spawn(Mod, Func, Args) ->
    Pid = spawn(Mod, Func, Args),
    on_exit(Pid, fun process_why/2),
    spawn(fun() ->
                  receive
                  after 1000 ->
                          exit(Pid, "Shutdown")
                  end end),
    Pid.

ticker() ->
    receive
    after 5000->
            io:format("I'm still running~n"),
            ticker()
    end.

spawn_ticker() ->
    io:format("Spawning ticker process"),
    register(tick5, Pid = spawn(fun ticker/0)),
    on_exit(Pid, fun(_Pid, _Why) ->
                         spawn_ticker() end),
    Pid.
            
noop() ->             
    receive
    after infinity->
            true
    end.
         
start_workers(Fs) ->
    Pids = [{spawn(F), F} || F <- Fs],
    [on_exit(Pid, F) || {Pid, F} <- Pids],
    Pids.
    
                                    
