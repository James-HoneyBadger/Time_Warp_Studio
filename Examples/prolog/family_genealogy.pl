% ============================================================
% FAMILY GENEALOGY — Prolog Language Showcase
% Extended family tree with relationship inference
% Time Warp Studio — Prolog Language Demo
% ============================================================

% ============================================================
% FAMILY DATABASE — 4 generations, 22 members
% ============================================================

% parent(Child, Parent)
parent(margaret, edward).
parent(margaret, eleanor).
parent(richard, edward).
parent(richard, eleanor).
parent(diana, edward).
parent(diana, eleanor).

parent(thomas, henry).
parent(thomas, grace).
parent(william, henry).
parent(william, grace).

parent(sophie, richard).
parent(sophie, helen).
parent(james, richard).
parent(james, helen).

parent(emily, thomas).
parent(emily, mary).
parent(oliver, thomas).
parent(oliver, mary).
parent(alice, thomas).
parent(alice, mary).

parent(charlie, william).
parent(charlie, lucy).
parent(ella, william).
parent(ella, lucy).

parent(lucas, james).
parent(lucas, patricia).
parent(noah, james).
parent(noah, patricia).

% gender facts
male(edward).   male(richard).  male(thomas).   male(william).
male(henry).    male(james).    male(charlie).  male(oliver).
male(lucas).    male(noah).

female(eleanor). female(diana).   female(margaret). female(grace).
female(helen).   female(sophie).  female(emily).    female(alice).
female(mary).    female(lucy).    female(ella).     female(patricia).

% ============================================================
% RELATIONSHIP RULES
% ============================================================

% Basic parent/child
mother(Child, Mother) :- parent(Child, Mother), female(Mother).
father(Child, Father) :- parent(Child, Father), male(Father).

% Grandparent
grandparent(Child, GP) :- parent(Child, P), parent(P, GP).
grandmother(Child, GM) :- grandparent(Child, GM), female(GM).
grandfather(Child, GF) :- grandparent(Child, GF), male(GF).

% Great-grandparent
great_grandparent(Child, GGP) :-
    parent(Child, P), parent(P, GP), parent(GP, GGP).

% Ancestor (n-th degree)
ancestor(Child, Anc) :- parent(Child, Anc).
ancestor(Child, Anc) :- parent(Child, Mid), ancestor(Mid, Anc).

% Sibling (same parents, different person)
sibling(X, Y) :- parent(X, P), parent(Y, P), X \= Y.

% Distinct sibling (avoid counting same pair from two parents)
distinct_sibling(X, Y) :- sibling(X, Y), X @< Y.

% Brother / Sister
brother(X, Y) :- sibling(X, Y), male(X).
sister(X, Y)  :- sibling(X, Y), female(X).

% Uncle / Aunt
uncle(Person, Uncle) :- parent(Person, Parent), brother(Uncle, Parent).
aunt(Person, Aunt)   :- parent(Person, Parent), sister(Aunt, Parent).

% Cousin (same-generation, different parents)
cousin(X, Y) :-
    parent(X, PX), parent(Y, PY),
    sibling(PX, PY), X \= Y.

% Nephew / Niece
nephew(Uncle, Nephew) :- uncle(Nephew, Uncle).
niece(Aunt, Niece)    :- aunt(Niece, Aunt).

% Generational depth
generation(Person, 0) :- \+ parent(Person, _).
generation(Person, D) :-
    parent(Person, Parent),
    generation(Parent, PD),
    D is PD + 1.

% Common ancestor
common_ancestor(X, Y, CA) :-
    ancestor(X, CA),
    ancestor(Y, CA).

% Closest common ancestor
% (shortest distance sum)
distance_to(Child, Anc, 0) :- Child = Anc.
distance_to(Child, Anc, D) :-
    parent(Child, P),
    distance_to(P, Anc, D1),
    D is D1 + 1.

% ============================================================
% QUERY HELPERS (print results nicely)
% ============================================================

:- meta_predicate forall(0, 0).

print_list([]).
print_list([H|T]) :-
    write('    - '), write(H), nl,
    print_list(T).

print_pairs([]).
print_pairs([[X,Y]|T]) :-
    write('    - '), write(X), write(' & '), write(Y), nl,
    print_pairs(T).

% ============================================================
% MAIN PROGRAM — Run queries and display results
% ============================================================

main :-
    write('============================================================'), nl,
    write('  FAMILY GENEALOGY — Prolog Showcase'), nl,
    write('  4-generation family tree with relationship inference'), nl,
    write('============================================================'), nl, nl,

    % --- Section 1: Direct relationships ---
    write('SECTION 1: DIRECT PARENT/CHILD'), nl,
    write('----------------------------------------------------------'), nl,
    findall(C-P, parent(C, P), Pairs),
    forall(member(C-P, Pairs),
           (write('  '), write(C), write(' has parent: '), write(P), nl)),
    nl,

    % --- Section 2: Siblings ---
    write('SECTION 2: SIBLING PAIRS'), nl,
    write('----------------------------------------------------------'), nl,
    findall([X,Y], distinct_sibling(X, Y), SibPairs),
    print_pairs(SibPairs), nl,

    % --- Section 3: Grandparent relationships ---
    write('SECTION 3: GRANDPARENT RELATIONSHIPS'), nl,
    write('----------------------------------------------------------'), nl,
    findall(C-GP, grandparent(C, GP), GPairs),
    forall(member(C-GP, GPairs),
           (write('  '), write(GP), write(' is grandparent of '), write(C), nl)),
    nl,

    % --- Section 4: Uncles and Aunts ---
    write('SECTION 4: UNCLES AND AUNTS'), nl,
    write('----------------------------------------------------------'), nl,
    write('  Uncles:'), nl,
    findall(P-U, uncle(P, U), UPairs),
    forall(member(P-U, UPairs),
           (write('    '), write(U), write(' is uncle of '), write(P), nl)),
    write('  Aunts:'), nl,
    findall(P-A, aunt(P, A), APairs),
    forall(member(P-A, APairs),
           (write('    '), write(A), write(' is aunt of '), write(P), nl)),
    nl,

    % --- Section 5: Cousins ---
    write('SECTION 5: COUSIN RELATIONSHIPS'), nl,
    write('----------------------------------------------------------'), nl,
    findall(X-Y, (cousin(X, Y), X @< Y), CPairs),
    (CPairs = [] ->
        write('  (no cousins found in data)') ;
        forall(member(X-Y, CPairs),
               (write('  '), write(X), write(' and '), write(Y), write(' are cousins'), nl))
    ),
    nl,

    % --- Section 6: Ancestors of specific people ---
    write('SECTION 6: ANCESTORS OF EMILY'), nl,
    write('----------------------------------------------------------'), nl,
    findall(A, ancestor(emily, A), EmilyAnc),
    sort(EmilyAnc, EmilyAncSorted),
    print_list(EmilyAncSorted), nl,

    write('SECTION 7: ANCESTORS OF LUCAS'), nl,
    write('----------------------------------------------------------'), nl,
    findall(A, ancestor(lucas, A), LucasAnc),
    sort(LucasAnc, LucasSorted),
    print_list(LucasSorted), nl,

    % --- Section 7: Generation depths ---
    write('SECTION 8: GENERATION DEPTHS'), nl,
    write('----------------------------------------------------------'), nl,
    findall(P-D, (generation(P, D)), GDeps),
    forall(member(P-D, GDeps),
           (write('  '), write(P), write(' — generation '), write(D), nl)),
    nl,

    % --- Section 8: Common ancestors ---
    write('SECTION 9: COMMON ANCESTORS'), nl,
    write('----------------------------------------------------------'), nl,
    write('  emily and charlie share ancestors:'), nl,
    findall(CA, common_ancestor(emily, charlie, CA), CAs),
    sort(CAs, CAsSorted),
    print_list(CAsSorted), nl,

    write('  lucas and oliver share ancestors:'), nl,
    findall(CA2, common_ancestor(lucas, oliver, CA2), CAs2),
    sort(CAs2, CAs2Sorted),
    print_list(CAs2Sorted), nl,

    % --- Section 9: All mothers and all fathers ---
    write('SECTION 10: ALL MOTHERS AND FATHERS'), nl,
    write('----------------------------------------------------------'), nl,
    write('  Mothers:'), nl,
    findall(M, (mother(_, M)), AllMoms), sort(AllMoms, AllMomsSorted),
    print_list(AllMomsSorted),
    write('  Fathers:'), nl,
    findall(F, (father(_, F)), AllDads), sort(AllDads, AllDadsSorted),
    print_list(AllDadsSorted), nl,

    write('============================================================'), nl,
    write('  Genealogy analysis complete!'), nl,
    write('  Inference engine derived all relationships from parent/2'), nl,
    write('============================================================'), nl.

:- initialization(main).
