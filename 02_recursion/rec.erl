-module(rec).

-export([fib/1, fac/1, b_print/2, b_print_s/2, b_print_t/2, digit_sum/1, rev_dig/1, triangle/1]).

% 1
fib(N) when not is_number(N); N < 0 ->
    io:format("Error~n");
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).


% 2
fac(N) when not is_number(N); N < 0 ->
    io:format("Error~n");
fac(0) -> 1;
fac(N) -> N * fac(N - 1).


% B: От A до B
b_print(A, B) when A > B ->
    b_print_back(A, B);
b_print(A, B) ->
    b_print_front(A, B).

b_print_front(A, B) when A - 1 == B -> ok;
b_print_front(A, B) ->
    io:format("~p~n", [A]),
    b_print_front(A + 1, B).

b_print_back(A, B) when A + 1 == B -> ok;
b_print_back(A, B) ->
    io:format("~p~n", [A]),
    b_print_back(A - 1, B).

% B 2
b_print_s(A, B) when A == B -> io:format("~p~n", [A]);
b_print_s(A, B) when A < B ->
    io:format("~p~n", [A]),
    b_print_s(A + 1, B);
b_print_s(A, B) when A > B ->
    io:format("~p~n", [A]),
    b_print_s(A - 1, B).


% B 3
% b_print_t(A, B) when A == B -> io:format("~p~n", [A]);
b_print_t(A, B) ->
    io:format("~p~n", [A]),
    if
        A < B -> b_print_t(A + 1, B);
        A > B -> b_print_t(A - 1, B);
        A == B -> ok
    end.


% E: Сумма цифр числа
% digit_sum(X) when X < 0 -> digit_sum(-1 * X);
digit_sum(X) when X < 10 -> X;
digit_sum(X) -> X rem 10 + digit_sum(X div 10).


% G: Цифры числа слева направо
% rev_dig(X) when X < 0 ->
    % io:format("-"),
    % rev_dig(-1 * X);
rev_dig(0) -> ok;
rev_dig(X) ->
    rev_dig(X div 10),
    io:format("~p ", [X rem 10]).


% R: Треугольная последовательность
triangle(0) -> ok;
triangle(X) ->
    triangle(X - 1),
    io:format("~s", [tri_str(X, X)]).

tri_str(_, I) when I == 0 -> "";
tri_str(X, I) ->
    integer_to_list(X) ++ " " ++ tri_str(X, I - 1).
