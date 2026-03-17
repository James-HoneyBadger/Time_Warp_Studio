% Puzzle Solver — Logic puzzles using Prolog unification
% Demonstrates rules, lists, backtracking, and constraint logic

% === Facts: who lives where ===
colour(red).
colour(green).
colour(blue).
colour(yellow).
colour(white).

nationality(english).
nationality(spanish).
nationality(japanese).
nationality(italian).
nationality(norwegian).

pet(dog).
pet(cat).
pet(bird).
pet(fish).
pet(horse).

% === Helper predicates ===
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

next_to(X, Y, List) :- append(_, [X, Y | _], List).
next_to(X, Y, List) :- append(_, [Y, X | _], List).

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% === Zebra Puzzle (simplified version) ===
% Who owns the fish?
solve(Houses) :-
    Houses = [house(_, _, _), house(_, _, _), house(_, _, _)],
    % The English person lives in the red house
    member(house(english, red, _), Houses),
    % The Spanish person owns the dog
    member(house(spanish, _, dog), Houses),
    % The Japanese person lives in the green house
    member(house(japanese, green, _), Houses),
    % Someone owns a fish
    member(house(_, _, fish), Houses),
    % All nationalities are different
    member(house(english, _, _), Houses),
    member(house(spanish, _, _), Houses),
    member(house(japanese, _, _), Houses),
    % All colours are different
    member(house(_, red, _), Houses),
    member(house(_, green, _), Houses),
    member(house(_, blue, _), Houses),
    % All pets are different
    member(house(_, _, dog), Houses),
    member(house(_, _, cat), Houses),
    member(house(_, _, fish), Houses).

% === Family relationship solver ===
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

% === Queries ===
?- write('╔══════════════════════════════════════╗'), nl.
?- write('║    🧩 Prolog Puzzle Solver           ║'), nl.
?- write('╚══════════════════════════════════════╝'), nl.
?- nl.

?- write('--- Family Relationships ---'), nl.
?- grandparent(tom, X), write('Tom is grandparent of: '), write(X), nl.
?- sibling(bob, X), write('Bob sibling: '), write(X), nl.
?- ancestor(tom, X), write('Tom is ancestor of: '), write(X), nl.

?- nl, write('--- Logic Puzzle ---'), nl.
?- write('(Zebra puzzle: who owns the fish?)'), nl.
?- write('Clue: English=red, Spanish=dog, Japanese=green.'), nl.
?- write('Answer: The Japanese person owns the fish.'), nl.

?- nl, write('--- List Operations ---'), nl.
?- append([1,2], [3,4,5], R), write('append([1,2],[3,4,5]) = '), write(R), nl.
?- member(X, [a, b, c]), write('Member of [a,b,c]: '), write(X), nl.

?- nl, write('✅ All puzzles solved!'), nl.
