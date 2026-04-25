# Erlang Hello World
# Time Warp Studio - Erlang example

-module(hello).
-export([main/0, greet/1, factorial/1]).

main() ->
    io:format("Hello, World!~n"),
    io:format("Erlang: concurrent, functional, fault-tolerant.~n"),

    % Greeting
    Name = "Time Warp",
    greet(Name),

    % Factorials
    io:format("~nFactorials:~n"),
    lists:foreach(
        fun(N) ->
            F = factorial(N),
            io:format("  ~w! = ~w~n", [N, F])
        end,
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    ),

    % List operations
    Numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    Evens = lists:filter(fun(X) -> X rem 2 == 0 end, Numbers),
    Squares = lists:map(fun(X) -> X * X end, Numbers),
    Sum = lists:sum(Numbers),

    io:format("~nNumbers: ~w~n", [Numbers]),
    io:format("Evens: ~w~n", [Evens]),
    io:format("Squares: ~w~n", [Squares]),
    io:format("Sum: ~w~n", [Sum]).

greet(Name) ->
    io:format("Welcome to ~s Studio!~n", [Name]).

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).
