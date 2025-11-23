%
% Recursion Examples
%
% Demonstrates:
% - Base cases and recursive cases
% - Mathematical recursion
% - List recursion
% - Tail recursion optimization
%

% Factorial: n! = n * (n-1)!
factorial(0, 1).
factorial(N, Result) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, Result1),
    Result is N * Result1.

% Fibonacci: fib(n) = fib(n-1) + fib(n-2)
fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, Result) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    Result is F1 + F2.

% Power: X^N
power(_, 0, 1).
power(X, N, Result) :-
    N > 0,
    N1 is N - 1,
    power(X, N1, Result1),
    Result is X * Result1.

% GCD (Greatest Common Divisor) - Euclidean algorithm
gcd(X, 0, X).
gcd(X, Y, Result) :-
    Y > 0,
    R is X mod Y,
    gcd(Y, R, Result).

% Count occurrences of element in list
count(_, [], 0).
count(X, [X|Tail], Count) :-
    count(X, Tail, TailCount),
    Count is TailCount + 1.
count(X, [Y|Tail], Count) :-
    X \= Y,
    count(X, Tail, Count).

% Range: generate list from M to N
range(M, N, []) :- M > N.
range(M, N, [M|Rest]) :-
    M =< N,
    M1 is M + 1,
    range(M1, N, Rest).

% Sum of digits
sum_digits(0, 0).
sum_digits(N, Sum) :-
    N > 0,
    Digit is N mod 10,
    Rest is N // 10,
    sum_digits(Rest, RestSum),
    Sum is Digit + RestSum.

% Palindrome check for lists
palindrome(L) :- reverse_helper(L, L).

reverse_helper([], []).
reverse_helper([H|T], Rev) :-
    reverse_helper(T, RevT),
    append_helper(RevT, [H], Rev).

append_helper([], L, L).
append_helper([H|T], L, [H|R]) :- append_helper(T, L, R).

% Flatten nested list
flatten([], []).
flatten([H|T], Flat) :-
    flatten(H, FlatH),
    flatten(T, FlatT),
    append_helper(FlatH, FlatT, Flat).
flatten(X, [X]) :- \+ is_list(X).

%
% Example queries:
%
% ?- factorial(5, F).               % Calculate 5!
% ?- fibonacci(10, F).              % 10th Fibonacci number
% ?- power(2, 8, P).                % Calculate 2^8
% ?- gcd(48, 18, G).                % GCD of 48 and 18
% ?- count(a, [a,b,a,c,a], N).      % Count 'a' in list
% ?- range(1, 10, L).               % Generate 1..10
% ?- sum_digits(12345, S).          % Sum of digits
% ?- palindrome([1,2,3,2,1]).       % Is it a palindrome?
%
