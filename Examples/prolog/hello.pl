% =============================================
%  Prolog Comprehensive Demo - Time Warp Studio
% =============================================

% --- Facts ---
parent(alice, bob).
parent(bob, charlie).
parent(bob, diana).
parent(alice, eve).

likes(alice, pizza).
likes(bob, sushi).
likes(charlie, pizza).

animal(dog).
animal(cat).
animal(bird).

% --- Rules ---
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
sibling(X, Y) :- parent(P, X), parent(P, Y), X \= Y.
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% --- Queries ---

% Simple fact queries
?- write('===== FACTS ====='), nl.
?- parent(alice, bob).
?- parent(bob, charlie).

% Variable binding
?- write('===== VARIABLES ====='), nl.
?- parent(alice, X).
?- parent(bob, X).

% Rule queries
?- write('===== RULES ====='), nl.
?- grandparent(alice, charlie).
?- sibling(charlie, diana).

% Arithmetic
?- write('===== ARITHMETIC ====='), nl.
?- X is 2 + 3, write(X), nl.
?- X is 10 * 5, write(X), nl.
?- X is 100 - 37, write(X), nl.
?- X is 15 // 4, write(X), nl.

% Comparison
?- write('===== COMPARISON ====='), nl.
?- 5 > 3.
?- 10 >= 10.
?- 3 < 7.

% List operations
?- write('===== LISTS ====='), nl.
?- member(2, [1, 2, 3]).
?- append([1, 2], [3, 4], X).
?- length([a, b, c], N), write(N), nl.
?- reverse([1, 2, 3], X).
?- last([10, 20, 30], X).

% Aggregation
?- write('===== AGGREGATION ====='), nl.
?- findall(X, animal(X), Animals).

% Unification
?- write('===== UNIFICATION ====='), nl.
?- X = hello, write(X), nl.
?- write([1, 2, 3]), nl.

% Type checking
?- write('===== TYPE CHECKS ====='), nl.
?- atom(hello).
?- number(42).
?- not(atom(42)).

% Control flow
?- write('===== CONTROL ====='), nl.
?- not(fail).

?- write('===== DONE ====='), nl.
