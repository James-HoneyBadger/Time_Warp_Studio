/* ============================================================
   KNOWLEDGE ENGINE — Prolog Logic Programming Showcase
   Animal Kingdom * Family Genealogy * Geographic Facts
   Time Warp Studio — Prolog Language Demo
   ============================================================ */

/* ===== ANIMAL KINGDOM ONTOLOGY ===== */

/* Classification hierarchy */
is_a(mammal, animal).
is_a(bird, animal).
is_a(reptile, animal).
is_a(fish, animal).
is_a(amphibian, animal).
is_a(insect, animal).

is_a(dog, mammal).
is_a(cat, mammal).
is_a(wolf, mammal).
is_a(lion, mammal).
is_a(dolphin, mammal).
is_a(bat, mammal).
is_a(elephant, mammal).
is_a(whale, mammal).
is_a(horse, mammal).
is_a(rabbit, mammal).

is_a(eagle, bird).
is_a(penguin, bird).
is_a(owl, bird).
is_a(parrot, bird).
is_a(ostrich, bird).
is_a(sparrow, bird).

is_a(cobra, reptile).
is_a(crocodile, reptile).
is_a(gecko, reptile).
is_a(iguana, reptile).

is_a(salmon, fish).
is_a(tuna, fish).
is_a(clownfish, fish).
is_a(shark, fish).

is_a(frog, amphibian).
is_a(salamander, amphibian).

is_a(butterfly, insect).
is_a(bee, insect).
is_a(ant, insect).

/* Properties */
has_property(dog, domestic).
has_property(cat, domestic).
has_property(horse, domestic).
has_property(rabbit, domestic).
has_property(parrot, domestic).

has_property(wolf, wild).
has_property(lion, wild).
has_property(eagle, wild).
has_property(cobra, wild).
has_property(shark, wild).
has_property(elephant, wild).
has_property(whale, wild).
has_property(dolphin, wild).
has_property(crocodile, wild).
has_property(butterfly, wild).

has_property(eagle, can_fly).
has_property(parrot, can_fly).
has_property(sparrow, can_fly).
has_property(owl, can_fly).
has_property(bat, can_fly).
has_property(butterfly, can_fly).
has_property(bee, can_fly).

has_property(penguin, cannot_fly).
has_property(ostrich, cannot_fly).

has_property(dolphin, can_swim).
has_property(whale, can_swim).
has_property(shark, can_swim).
has_property(salmon, can_swim).
has_property(penguin, can_swim).
has_property(crocodile, can_swim).
has_property(frog, can_swim).

has_property(whale, endangered).
has_property(elephant, endangered).
has_property(eagle, endangered).

/* Transitive classification */
is_type(X, Y) :- is_a(X, Y).
is_type(X, Y) :- is_a(X, Z), is_type(Z, Y).

/* ===== FAMILY GENEALOGY ===== */

/* parent(Parent, Child) */
parent(george, charles).
parent(george, anne).
parent(george, andrew).
parent(george, edward).
parent(elizabeth, charles).
parent(elizabeth, anne).
parent(elizabeth, andrew).
parent(elizabeth, edward).
parent(charles, william).
parent(charles, harry).
parent(diana, william).
parent(diana, harry).
parent(william, george_jr).
parent(william, charlotte).
parent(william, louis).
parent(kate, george_jr).
parent(kate, charlotte).
parent(kate, louis).
parent(harry, archie).
parent(meghan, archie).

male(george). male(charles). male(andrew). male(edward).
male(william). male(harry). male(george_jr). male(louis). male(archie).
female(elizabeth). female(anne). female(diana). female(kate).
female(meghan). female(charlotte).

/* Derived relationships */
father(F, C) :- parent(F, C), male(F).
mother(M, C) :- parent(M, C), female(M).

grandparent(GP, GC) :- parent(GP, P), parent(P, GC).
grandfather(GF, GC) :- grandparent(GF, GC), male(GF).
grandmother(GM, GC) :- grandparent(GM, GC), female(GM).

sibling(X, Y) :- parent(P, X), parent(P, Y), X \= Y.
brother(X, Y) :- sibling(X, Y), male(X).
sister(X, Y) :- sibling(X, Y), female(X).

ancestor(A, D) :- parent(A, D).
ancestor(A, D) :- parent(A, M), ancestor(M, D).

/* ===== GEOGRAPHIC KNOWLEDGE BASE ===== */

capital(france, paris).
capital(germany, berlin).
capital(spain, madrid).
capital(italy, rome).
capital(uk, london).
capital(japan, tokyo).
capital(china, beijing).
capital(usa, washington).
capital(australia, canberra).
capital(brazil, brasilia).
capital(india, delhi).
capital(canada, ottawa).
capital(russia, moscow).
capital(argentina, buenos_aires).
capital(egypt, cairo).

continent(france, europe).
continent(germany, europe).
continent(spain, europe).
continent(italy, europe).
continent(uk, europe).
continent(japan, asia).
continent(china, asia).
continent(india, asia).
continent(russia, europe). /* also asia — simplified */
continent(usa, north_america).
continent(canada, north_america).
continent(mexico, north_america).
continent(brazil, south_america).
continent(argentina, south_america).
continent(australia, oceania).
continent(egypt, africa).
continent(nigeria, africa).
continent(kenya, africa).

population_millions(china, 1400).
population_millions(india, 1380).
population_millions(usa, 331).
population_millions(brazil, 213).
population_millions(russia, 145).
population_millions(japan, 126).
population_millions(germany, 83).
population_millions(uk, 67).
population_millions(france, 67).
population_millions(australia, 25).

borders(france, germany).
borders(france, spain).
borders(france, italy).
borders(germany, netherlands).
borders(germany, poland).
borders(germany, austria).
borders(usa, canada).
borders(usa, mexico).
borders(X, Y) :- borders(Y, X).  /* symmetric */

/* ===== RULES AND QUERIES ===== */

/* Animals that can fly but are not birds */
flying_non_bird(X) :-
    has_property(X, can_fly),
    \+ is_type(X, bird).

/* Animals that can swim but are not fish */
swimming_non_fish(X) :-
    has_property(X, can_swim),
    \+ is_type(X, fish).

/* Countries sharing a continent */
same_continent(X, Y) :-
    continent(X, C),
    continent(Y, C),
    X \= Y.

/* Generations down from an ancestor */
generation(A, D, 1) :- parent(A, D).
generation(A, D, N) :-
    parent(A, M),
    generation(M, D, N1),
    N is N1 + 1.

/* ===== MAIN QUERIES ===== */

?- writeln('============================================================').
?- writeln('  KNOWLEDGE ENGINE — Prolog Logic Programming Showcase').
?- writeln('  Backtracking * Unification * Rule Inference').
?- writeln('============================================================').
?- nl.

?- writeln('[ 1 ] ANIMAL CLASSIFICATION').
?- writeln('  All mammals in our knowledge base:').
?- findall(M, is_type(M, mammal), Ms), writeln(Ms).
?- nl.

?- writeln('  Flying animals (includes non-birds!):').
?- findall(F, has_property(F, can_fly), Fs), writeln(Fs).
?- nl.

?- writeln('  Animals that can fly but are NOT birds:').
?- findall(X, flying_non_bird(X), Xs), writeln(Xs).
?- nl.

?- writeln('  Animals that can swim but are NOT fish:').
?- findall(X, swimming_non_fish(X), Xs), writeln(Xs).
?- nl.

?- writeln('  Endangered animals:').
?- findall(E, has_property(E, endangered), Es), writeln(Es).
?- nl.

?- writeln('  Domestic animals:').
?- findall(D, has_property(D, domestic), Ds), writeln(Ds).
?- nl.

?- writeln('[ 2 ] FAMILY GENEALOGY').
?- writeln('  Children of george:').
?- findall(C, parent(george, C), Cs), writeln(Cs).
?- nl.

?- writeln('  Grandchildren of elizabeth:').
?- findall(GC, grandparent(elizabeth, GC), GCs), writeln(GCs).
?- nl.

?- writeln('  All ancestors of archie:').
?- findall(A, ancestor(A, archie), As), writeln(As).
?- nl.

?- writeln('  Great-grandchildren of george (generation 3):').
?- findall(D, generation(george, D, 3), Ds), writeln(Ds).
?- nl.

?- writeln('  Siblings of william:').
?- findall(S, sibling(william, S), Ss), sort(Ss, Sorted), writeln(Sorted).
?- nl.

?- writeln('[ 3 ] GEOGRAPHIC KNOWLEDGE').
?- writeln('  All European countries and their capitals:').
?- findall(C-Cap, (continent(C, europe), capital(C, Cap)), Pairs),
   maplist(writeln, Pairs).
?- nl.

?- writeln('  Countries that border France:').
?- findall(B, borders(france, B), Bs), writeln(Bs).
?- nl.

?- writeln('  Countries in Asia:').
?- findall(C, continent(C, asia), Cs), writeln(Cs).
?- nl.

?- writeln('  Countries with population > 200 million:').
?- findall(C-P, (population_millions(C, P), P > 200), Pairs), writeln(Pairs).
?- nl.

?- writeln('[ 4 ] LOGIC PUZZLES — Backtracking').
?- writeln('  Safe coloring: no two bordering European countries same color').
?- writeln('  (3-coloring for france, germany, spain, italy)').

color(red). color(blue). color(green).

different(X, Y) :- X \= Y.

color_europe(CF, CG, CS, CI) :-
    color(CF), color(CG), color(CS), color(CI),
    different(CF, CG),   % france-germany border
    different(CF, CS),   % france-spain border
    different(CF, CI),   % france-italy border
    different(CG, CI).   % germany-italy (via austria, close)

?- findall(f(CF,CG,CS,CI),
           color_europe(CF,CG,CS,CI),
           Solutions),
   length(Solutions, N),
   format("  Found ~w valid colorings~n", [N]),
   Solutions = [First|_],
   First = f(CF,CG,CS,CI),
   format("  Example: france=~w germany=~w spain=~w italy=~w~n",
          [CF,CG,CS,CI]).
?- nl.

?- writeln('[ 5 ] META-REASONING').
?- writeln('  How many facts are in the knowledge base?').
?- aggregate_all(count, is_a(_, _), IsA),
   format("  is_a facts: ~w~n", [IsA]).
?- aggregate_all(count, has_property(_, _), Props),
   format("  has_property facts: ~w~n", [Props]).
?- aggregate_all(count, parent(_, _), Parens),
   format("  parent facts: ~w~n", [Parens]).
?- aggregate_all(count, capital(_, _), Caps),
   format("  capital facts: ~w~n", [Caps]).
?- nl.

?- writeln('============================================================').
?- writeln('  Knowledge Engine Complete!').
?- writeln('  Prolog: the language where programs are theorems.').
?- writeln('============================================================').
