%
% Family Tree - Relationships and Queries
%
% Demonstrates:
% - Facts about family members
% - Rules defining relationships
% - Complex queries
% - Recursive definitions
%

% Facts: parent(Parent, Child)
parent(john, mary).
parent(john, tom).
parent(susan, mary).
parent(susan, tom).
parent(mary, alice).
parent(mary, bob).
parent(tom, charlie).
parent(jane, alice).
parent(jane, bob).
parent(robert, charlie).

% Facts: male and female
male(john).
male(tom).
male(bob).
male(charlie).
male(robert).

female(susan).
female(mary).
female(jane).
female(alice).

% Rules: Basic family relationships

% Someone is a mother if they are female and a parent
mother(Mother, Child) :- parent(Mother, Child), female(Mother).

% Someone is a father if they are male and a parent
father(Father, Child) :- parent(Father, Child), male(Father).

% Siblings share a parent
sibling(X, Y) :- 
    parent(P, X), 
    parent(P, Y), 
    X \= Y.

% Grandparent relationship
grandparent(GP, GC) :- parent(GP, P), parent(P, GC).

grandmother(GM, GC) :- grandparent(GM, GC), female(GM).
grandfather(GF, GC) :- grandparent(GF, GC), male(GF).

% Uncle or aunt
uncle(Uncle, Niece) :- 
    parent(P, Niece),
    sibling(Uncle, P),
    male(Uncle).

aunt(Aunt, Niece) :-
    parent(P, Niece),
    sibling(Aunt, P),
    female(Aunt).

% Cousin relationship
cousin(X, Y) :-
    parent(PX, X),
    parent(PY, Y),
    sibling(PX, PY).

% Ancestor (recursive definition)
ancestor(A, D) :- parent(A, D).
ancestor(A, D) :- parent(A, X), ancestor(X, D).

%
% Example queries:
%
% ?- mother(susan, mary).           % Is Susan Mary's mother?
% ?- father(X, alice).              % Who is Alice's father?
% ?- sibling(mary, tom).            % Are Mary and Tom siblings?
% ?- grandparent(john, alice).      % Is John Alice's grandparent?
% ?- ancestor(john, charlie).       % Is John an ancestor of Charlie?
% ?- cousin(alice, charlie).        % Are Alice and Charlie cousins?
%
