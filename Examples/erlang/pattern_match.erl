# Erlang Pattern Matching Demo
# Time Warp Studio - Pattern matching and recursion

-module(pattern_match).
-export([main/0]).

% Describe a shape using pattern matching
describe({circle, R}) ->
    Area = 3.14159 * R * R,
    io:format("Circle with radius ~w, area = ~w~n", [R, Area]);
describe({rectangle, W, H}) ->
    Area = W * H,
    io:format("Rectangle ~wx~w, area = ~w~n", [W, H, Area]);
describe({triangle, B, H}) ->
    Area = 0.5 * B * H,
    io:format("Triangle base ~w height ~w, area = ~w~n", [B, H, Area]);
describe(Unknown) ->
    io:format("Unknown shape: ~w~n", [Unknown]).

% FizzBuzz using pattern matching
fizzbuzz(N) when N rem 15 == 0 -> "FizzBuzz";
fizzbuzz(N) when N rem 3 == 0  -> "Fizz";
fizzbuzz(N) when N rem 5 == 0  -> "Buzz";
fizzbuzz(N) -> integer_to_list(N).

% Binary tree operations
insert(X, nil) -> {node, X, nil, nil};
insert(X, {node, Y, L, R}) when X < Y -> {node, Y, insert(X, L), R};
insert(X, {node, Y, L, R}) when X > Y -> {node, Y, L, insert(X, R)};
insert(X, {node, X, L, R}) -> {node, X, L, R}.

inorder(nil) -> [];
inorder({node, X, L, R}) -> inorder(L) ++ [X] ++ inorder(R).

main() ->
    io:format("=== Pattern Matching Demo ===~n~n"),

    Shapes = [{circle, 5}, {rectangle, 4, 6}, {triangle, 3, 8}, {pentagon, 5}],
    io:format("Shapes:~n"),
    lists:foreach(fun describe/1, Shapes),

    io:format("~nFizzBuzz 1-20:~n"),
    Results = lists:map(fun fizzbuzz/1, lists:seq(1, 20)),
    io:format("~s~n", [string:join(Results, " ")]),

    io:format("~nBinary Search Tree:~n"),
    Values = [5, 3, 7, 1, 4, 6, 8, 2],
    Tree = lists:foldl(fun insert/2, nil, Values),
    Sorted = inorder(Tree),
    io:format("Inserted: ~w~n", [Values]),
    io:format("Sorted:   ~w~n", [Sorted]).
