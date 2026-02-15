% Time Warp Studio - Prolog Demo
% Demonstrating Logic Programming

% Facts - Family relationships
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

% Properties
male(tom).
male(bob).
male(pat).
male(jim).
female(liz).
female(ann).

% Rules
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
father(X, Y) :- parent(X, Y), male(X).
mother(X, Y) :- parent(X, Y), female(X).

% Example queries:
% parent(tom, X) - Find tom's children
% grandparent(tom, X) - Find tom's grandchildren
% father(X, ann) - Find ann's father
% male(X) - Find all males

% Display message
:- write('Prolog Demo Loaded'), nl, write('Facts and rules loaded'), nl.
