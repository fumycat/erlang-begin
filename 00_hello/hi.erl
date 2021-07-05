-module(hi).

-export([num_test/0, greet/0]).

% https://erlang.org/doc/man/io.html
num_test() ->
    io:format("~p~n~p~n", [100 / 3, 20.0 * 2]).

greet() ->
    Name = io:get_line("What's your name? "),
    Number = io:get_line("Enter your phone number: "),
    io:format("Hello!~nName: ~sNumber: ~s", [Name, Number]).
