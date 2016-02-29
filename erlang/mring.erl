-module(mring).
-export([start/1, stop/1, sendMsg/3, test/2]).

start(0) ->
    [];
start(N) ->
    Pid = spawn(fun() -> wait(N) end),
    io:format("spawned ~w with pid ~w~n", [N, Pid]),
    lists:append(start(N-1), [Pid]).

stop(L)->
    lists:foreach(fun(X) -> X ! stop end, L).
                          
                   
    
wait(N) ->
    receive
        {message, PidList, M}->
            if 
                N > 1 ->
                    Pid = lists:nth(N-1, PidList),
                    Pid ! {message, PidList, M};
                true ->
                    io:format("~w here is the message ~w~n",[self(), M])
            end,
            wait(N);
        stop ->
            io:format("ticker ~w stopped ~n", [N]),
            void
    end.
    

sendMsg(_P, _M, 0) ->
    void;

sendMsg(P, M, Times) ->
    Pid = lists:nth(length(P), P),
    io:format("send message to ~w~n", [Pid]),
    Pid ! {message, P, M},
    sendMsg(P,M,Times-1).

test(N, Times) ->
    L = start(N),
    sendMsg(L, "s", Times),
    L.
    
