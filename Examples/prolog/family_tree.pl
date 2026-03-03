/* =====================================================
   FAMILY TREE & GENEALOGY SYSTEM in Prolog
   A comprehensive family knowledge base with:
   - Ancestry queries  
   - Relationship detection
   - Inheritance rules
   - Export and search
   
   Demonstrates: facts, rules, recursion, negation,
   lists, arithmetic, cut operator.
   ===================================================== */

/* ─── FAMILY FACTS ─────────────────────────────────── */
/* parent(Parent, Child) */
parent(george, charles).
parent(george, anne).
parent(george, andrew).
parent(george, edward).
parent(diana, charles).
parent(diana, anne).
parent(charles, william).
parent(charles, harry).
parent(camilla, william).
parent(camilla, harry).
parent(william, george_jr).
parent(william, charlotte).
parent(william, louis).
parent(kate, george_jr).
parent(kate, charlotte).
parent(kate, louis).
parent(harry, archie).
parent(harry, lili).
parent(meghan, archie).
parent(meghan, lili).

/* gender */
male(george).   female(diana).
male(charles).  female(anne).
male(andrew).   male(edward).
male(william).  male(harry).
female(camilla). female(kate).
female(meghan).
male(george_jr). female(charlotte). male(louis).
male(archie).   female(lili).

/* birth years */
born(george, 1948).
born(diana, 1961).
born(charles, 1948).
born(anne, 1950).
born(andrew, 1960).
born(edward, 1964).
born(william, 1982).
born(harry, 1984).
born(george_jr, 2013).
born(charlotte, 2015).
born(louis, 2018).
born(archie, 2019).
born(lili, 2021).

/* ─── BASIC RELATIONSHIP RULES ─────────────────────── */

father(F, C) :- parent(F, C), male(F).
mother(M, C) :- parent(M, C), female(M).

sibling(X, Y) :-
    parent(P, X),
    parent(P, Y),
    X \= Y.

brother(X, Y) :- sibling(X, Y), male(X).
sister(X, Y)  :- sibling(X, Y), female(X).

grandparent(GP, GC) :-
    parent(GP, P),
    parent(P, GC).

grandfather(GF, GC) :- grandparent(GF, GC), male(GF).
grandmother(GM, GC) :- grandparent(GM, GC), female(GM).

/* ─── RECURSIVE ANCESTRY ─────────────────────────────── */

ancestor(A, D) :- parent(A, D).
ancestor(A, D) :- parent(A, X), ancestor(X, D).

descendant(D, A) :- ancestor(A, D).

/* ─── EXTENDED RELATIONSHIPS ──────────────────────────── */

uncle(U, N) :-
    parent(P, N),
    brother(U, P).

aunt(A, N) :-
    parent(P, N),
    sister(A, P).

cousin(X, Y) :-
    parent(PX, X),
    parent(PY, Y),
    sibling(PX, PY).

/* ─── GENERATION DISTANCE ─────────────────────────────── */

generation(A, D, 1) :- parent(A, D), !.
generation(A, D, N) :-
    parent(A, X),
    generation(X, D, N1),
    N is N1 + 1.

/* ─── AGE QUERIES ──────────────────────────────────────── */

age(Person, Age) :-
    born(Person, Year),
    Age is 2025 - Year.

older_than(X, Y) :-
    age(X, AX),
    age(Y, AY),
    AX > AY.

/* ─── FAMILY COUNTS ─────────────────────────────────────── */

count_children(Parent, Count) :-
    findall(C, parent(Parent, C), Children),
    length(Children, Count).

count_descendants(Person, Count) :-
    findall(D, descendant(D, Person), Descs),
    length(Descs, Count).

/* ─── DEMO QUERIES ──────────────────────────────────────── */

:- write('=== FAMILY TREE DEMO ==='), nl.
:- write(''), nl.

:- write('--- George''s children ---'), nl.
:- forall(parent(george, C), (write('  '), write(C), nl)).

:- write(''), nl.
:- write('--- William''s children ---'), nl.
:- forall(parent(william, C), (write('  '), write(C), nl)).

:- write(''), nl.
:- write('--- George Jr''s grandparents ---'), nl.
:- forall(grandparent(GP, george_jr), (write('  '), write(GP), nl)).

:- write(''), nl.
:- write('--- George Jr''s ancestors ---'), nl.
:- forall(ancestor(A, george_jr), (write('  '), write(A), nl)).

:- write(''), nl.
:- write('--- Siblings of charles ---'), nl.
:- forall(sibling(S, charles), (write('  '), write(S), nl)).

:- write(''), nl.
:- write('--- All cousins ---'), nl.
:- forall(cousin(X, Y),
    (X @< Y, write('  '), write(X), write(' and '), write(Y), nl)).

:- write(''), nl.
:- write('--- Ages ---'), nl.
:- forall(born(P, _), (age(P, A), write('  '), write(P), write(': '), write(A), nl)).

:- write(''), nl.
:- write('--- George''s descendants count ---'), nl.
:- count_descendants(george, N), write('  Total descendants: '), write(N), nl.

:- write(''), nl.
:- write('--- Generation from George to Archie ---'), nl.
:- generation(george, archie, G), write('  Generation gap: '), write(G), nl.

:- write(''), nl.
:- write('=== PROLOG LOGIC QUERY DEMO ==='), nl.

/* Can we prove William is a descendant of George? */
:- write('  william descendant of george? '),
   (descendant(william, george) -> write('YES') ; write('NO')), nl.

/* Is Archie a descendant of Diana? */
:- write('  archie descendant of diana? '),
   (descendant(archie, diana) -> write('YES') ; write('NO')), nl.

/* Is Charlotte older than Louis? */
:- write('  charlotte older than louis? '),
   (older_than(charlotte, louis) -> write('YES') ; write('NO')), nl.

:- write(''), nl.
:- write('Family tree complete!'), nl.
