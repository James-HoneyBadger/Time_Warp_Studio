%
% Arithmetic and Mathematical Operations
%
% Demonstrates:
% - Arithmetic operators
% - Comparison operators
% - Mathematical predicates
% - Number manipulation
%

% Check if number is even or odd
is_even(N) :- 0 is N mod 2.
is_odd(N) :- 1 is N mod 2.

% Absolute value
abs_value(X, X) :- X >= 0.
abs_value(X, Y) :- X < 0, Y is -X.

% Maximum of two numbers
max_num(X, Y, X) :- X >= Y.
max_num(X, Y, Y) :- Y > X.

% Minimum of two numbers
min_num(X, Y, X) :- X =< Y.
min_num(X, Y, Y) :- Y < X.

% Check if number is prime
is_prime(2).
is_prime(N) :- 
    N > 2,
    is_odd(N),
    \+ has_divisor(N, 3).

has_divisor(N, D) :-
    D * D =< N,
    0 is N mod D.
has_divisor(N, D) :-
    D * D =< N,
    D2 is D + 2,
    has_divisor(N, D2).

% Calculate average of list
average(List, Avg) :-
    sum_list_nums(List, Sum),
    length(List, Len),
    Avg is Sum / Len.

sum_list_nums([], 0).
sum_list_nums([H|T], Sum) :-
    sum_list_nums(T, RestSum),
    Sum is H + RestSum.

% Find all divisors of a number
divisors(N, Divisors) :- 
    findall(D, (between(1, N, D), 0 is N mod D), Divisors).

% Perfect number (sum of divisors equals number)
is_perfect(N) :-
    N > 1,
    divisors(N, Divs),
    sum_without_n(Divs, N, Sum),
    Sum =:= N.

sum_without_n([], _, 0).
sum_without_n([N|T], N, Sum) :- sum_without_n(T, N, Sum).
sum_without_n([H|T], N, Sum) :-
    H \= N,
    sum_without_n(T, N, RestSum),
    Sum is H + RestSum.

% Triangle number: T(n) = n*(n+1)/2
triangle(N, T) :- T is N * (N + 1) // 2.

% Square number check
is_square(N) :-
    Sqrt is sqrt(N),
    Integer is floor(Sqrt),
    N =:= Integer * Integer.

% Collatz sequence length
collatz_length(1, 0).
collatz_length(N, Length) :-
    N > 1,
    (is_even(N) ->
        N1 is N // 2
    ;
        N1 is 3 * N + 1
    ),
    collatz_length(N1, L1),
    Length is L1 + 1.

%
% Example queries:
%
% ?- is_even(10).                   % Is 10 even?
% ?- is_prime(17).                  % Is 17 prime?
% ?- average([1,2,3,4,5], A).       % Average of list
% ?- divisors(12, D).               % Divisors of 12
% ?- is_perfect(6).                 % Is 6 perfect?
% ?- triangle(10, T).               % 10th triangle number
% ?- is_square(16).                 % Is 16 a square?
% ?- collatz_length(27, L).         % Collatz length for 27
%
