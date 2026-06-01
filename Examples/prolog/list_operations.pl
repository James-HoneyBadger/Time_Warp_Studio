% Prolog List Operations
% Demonstrates append, member, length, reverse, nth, flatten, and permutations

% ── Append ─────────────────────────────────────────────────────────────
my_append([], L, L).
my_append([H|T], L, [H|R]) :- my_append(T, L, R).

% ── Member ─────────────────────────────────────────────────────────────
my_member(X, [X|_]).
my_member(X, [_|T]) :- my_member(X, T).

% ── Length ─────────────────────────────────────────────────────────────
my_length([], 0).
my_length([_|T], N) :- my_length(T, N1), N is N1 + 1.

% ── Reverse ────────────────────────────────────────────────────────────
my_reverse([], []).
my_reverse([H|T], R) :- my_reverse(T, RT), my_append(RT, [H], R).

% ── Nth element (1-based) ──────────────────────────────────────────────
my_nth(1, [H|_], H).
my_nth(N, [_|T], X) :- N > 1, N1 is N - 1, my_nth(N1, T, X).

% ── Last element ───────────────────────────────────────────────────────
my_last([X], X).
my_last([_|T], X) :- my_last(T, X).

% ── Flatten nested list ────────────────────────────────────────────────
my_flatten([], []).
my_flatten([H|T], F) :-
    is_list(H), !,
    my_flatten(H, FH),
    my_flatten(T, FT),
    my_append(FH, FT, F).
my_flatten([H|T], [H|FT]) :- my_flatten(T, FT).

% ── Delete first occurrence ─────────────────────────────────────────────
my_delete(X, [X|T], T).
my_delete(X, [H|T], [H|R]) :- my_delete(X, T, R).

% ── Permutation ────────────────────────────────────────────────────────
my_permutation([], []).
my_permutation(L, [H|T]) :-
    my_delete(H, L, Rest),
    my_permutation(Rest, T).

% ── Demos ──────────────────────────────────────────────────────────────
:- initialization(main).

main :-
    writeln('=== Prolog List Operations ==='),

    % Append
    my_append([1,2,3], [4,5,6], App),
    write('append([1,2,3],[4,5,6]) = '), writeln(App),

    % Member
    ( my_member(3, [1,2,3,4,5]) -> write('3 is member: true') ; write('3 is member: false') ),
    nl,
    ( my_member(9, [1,2,3,4,5]) -> write('9 is member: true') ; write('9 is member: false') ),
    nl,

    % Length
    my_length([a,b,c,d,e], Len),
    write('length([a,b,c,d,e]) = '), writeln(Len),

    % Reverse
    my_reverse([1,2,3,4,5], Rev),
    write('reverse([1,2,3,4,5]) = '), writeln(Rev),

    % Nth
    my_nth(3, [10,20,30,40,50], E),
    write('nth(3,[10,20,30,40,50]) = '), writeln(E),

    % Last
    my_last([1,2,3,4,5], Last),
    write('last([1,2,3,4,5]) = '), writeln(Last),

    % Flatten
    my_flatten([1,[2,[3,4]],5,[6]], Flat),
    write('flatten([1,[2,[3,4]],5,[6]]) = '), writeln(Flat),

    % Permutations of [1,2,3]
    writeln('\nAll permutations of [1,2,3]:'),
    forall(my_permutation([1,2,3], P), (write('  '), writeln(P))).
