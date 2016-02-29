-module(area_server0).
-export([loop/0, area/2, start/0, sleep/1, flush_buffer/0]).

start() ->
    spawn(area_server0, loop, []).

area(Pid, What)->
    rpc(Pid, What).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.

loop() ->
    receive
        {From, {rectangle, Width, Ht}} ->
            From ! {self(), Width * Ht},
            loop();
        {From, {square, Side}} ->
            From ! {self(), Side * Side},
            loop();
        {From, {circle, Radius}} ->
            From ! {self(), 3.14 * Radius * Radius},
            loop()
    end.


sleep(T) ->
    receive
    after T ->
            true
    end.

flush_buffer() ->
    receive
        _Any ->
            flush_buffer()
    after 0->
        true
    end.

priority_receive() ->
    receive
        {alarm, X} ->
            {alarm, X}
    after 0 ->
            receive
                Any -> Any
            end
    end.
                    
                        
            
