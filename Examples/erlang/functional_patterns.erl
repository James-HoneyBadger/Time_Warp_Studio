# Erlang Functional Patterns
# Time Warp Studio - Higher-order functions, list comprehensions, closures

-module(functional_patterns).
-export([main/0]).

% ── Higher-order list operations ─────────────────────────────────────────────

% Compose two functions: compose(F, G)(X) = F(G(X))
compose(F, G) -> fun(X) -> F(G(X)) end.

% Curry a 2-argument function
curry(F) -> fun(X) -> fun(Y) -> F(X, Y) end end.

% Apply a function N times
apply_n(_, X, 0) -> X;
apply_n(F, X, N) -> apply_n(F, F(X), N - 1).

% Zip two lists together
zip([], _) -> [];
zip(_, []) -> [];
zip([H1|T1], [H2|T2]) -> [{H1, H2} | zip(T1, T2)].

% Flatten a nested list (one level)
flatten([]) -> [];
flatten([H|T]) when is_list(H) -> H ++ flatten(T);
flatten([H|T]) -> [H | flatten(T)].

% Running sum (scan left)
scan_sum([]) -> [];
scan_sum(List) -> scan_sum(List, 0, []).
scan_sum([], _, Acc) -> lists:reverse(Acc);
scan_sum([H|T], Sum, Acc) -> scan_sum(T, Sum + H, [Sum + H | Acc]).

% Group consecutive equal elements
group([]) -> [];
group([H|T]) -> group(T, H, 1, []).
group([], Last, Count, Acc) -> lists:reverse([{Last, Count} | Acc]);
group([H|T], H, Count, Acc) -> group(T, H, Count + 1, Acc);
group([H|T], Last, Count, Acc) -> group(T, H, 1, [{Last, Count} | Acc]).

% ── Main ─────────────────────────────────────────────────────────────────────

main() ->
    io:format("=== Erlang Functional Patterns ===~n~n"),

    %% --- Function composition ---
    io:format("Function Composition:~n"),
    Double = fun(X) -> X * 2 end,
    Inc    = fun(X) -> X + 1 end,
    DoubleInc = compose(Double, Inc),   % Double(Inc(X))
    IncDouble = compose(Inc, Double),   % Inc(Double(X))
    io:format("  double(inc(4))  = ~w~n", [DoubleInc(4)]),
    io:format("  inc(double(4))  = ~w~n", [IncDouble(4)]),

    %% --- Apply N times ---
    io:format("~nApply N times:~n"),
    Triple = apply_n(Double, 1, 3),  % 2^3
    io:format("  double applied 3 times to 1 = ~w~n", [Triple]),

    %% --- List comprehensions ---
    io:format("~nList Comprehensions:~n"),
    Squares = [X * X || X <- lists:seq(1, 8)],
    io:format("  Squares 1-8: ~w~n", [Squares]),
    Pairs = [{X, Y} || X <- [1,2,3], Y <- [a,b], X rem 2 =:= 1],
    io:format("  Odd-index pairs: ~w~n", [Pairs]),
    Pythagorean = [{A,B,C} || A <- lists:seq(1,15), B <- lists:seq(A,15), C <- lists:seq(B,15), A*A + B*B =:= C*C],
    io:format("  Pythagorean triples (<=15): ~w~n", [Pythagorean]),

    %% --- Zip and scan ---
    io:format("~nZip & Running Sum:~n"),
    Zipped = zip([a,b,c,d], [1,2,3,4]),
    io:format("  zip([a,b,c,d],[1..4]) = ~w~n", [Zipped]),
    Running = scan_sum([1,2,3,4,5,6]),
    io:format("  running sum [1..6] = ~w~n", [Running]),

    %% --- Flatten ---
    io:format("~nFlatten:~n"),
    Nested = [[1,2,3], [4,5], [6,7,8,9]],
    Flat = flatten(Nested),
    io:format("  ~w~n  -> ~w~n", [Nested, Flat]),

    %% --- Grouping ---
    io:format("~nRun-length Encoding:~n"),
    Word = [a,a,b,b,b,c,a,a],
    Grouped = group(Word),
    io:format("  ~w~n  -> ~w~n", [Word, Grouped]),

    %% --- Currying ---
    io:format("~nCurried Functions:~n"),
    Add = curry(fun(X, Y) -> X + Y end),
    Add5 = Add(5),
    Results = lists:map(Add5, [1,2,3,4,5]),
    io:format("  add5 applied to [1..5]: ~w~n", [Results]),

    io:format("~nDone.~n").
