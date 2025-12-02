% Time Warp Prolog Showcase
% Demonstrates facts, rules, queries, recursion and simple reasoning

% Family facts
parent(alice, bob).
parent(bob, carol).
parent(carol, dave).
parent(dave, erin).

% Sibling facts
sibling(bob, betty).

% Rules
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
ancestor(X, Z) :- parent(X, Z).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).

% Count descendants
descendants_count(X, Count) :- findall(Y, ancestor(X, Y), L), length(L, Count).

% Example queries (for interactive use):
% ?- grandparent(alice, carol).
% ?- ancestor(alice, erin).
% ?- descendants_count(alice, N).

% Small answer-generating rule
is_older(bob, carol).

% Demonstration: a simple family tree printer rule
print_family :-
  write('Family relationships:'), nl,
  forall(parent(A,B), (write(A), write(' -> '), write(B), nl)).
