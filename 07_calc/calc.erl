-module(calc).

-export([run/0]).

proc([pow, X, Y]) ->
    math:pow(X, Y);
proc([sqrt, X]) ->
    math:sqrt(X);
proc(_) ->
    "Bad arguments".

run() ->
    receive
        {From, Message} ->
            io:format("~p~n", [Message]),
            From ! proc(Message),
            run();
        _ ->
            io:format("Stop!~n")
    end.
