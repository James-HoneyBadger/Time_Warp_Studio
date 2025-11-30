%
% Facts and Rules - Basic Knowledge Base
%
% This program demonstrates:
% - Defining facts
% - Defining rules
% - Querying the knowledge base
%

% Facts: Direct statements of truth
likes(alice, programming).
likes(bob, music).
likes(charlie, sports).
likes(alice, puzzles).
likes(bob, programming).

% Facts about activities
fun(programming).
fun(music).
fun(sports).
fun(puzzles).

% Rules: Logical relationships
% Someone is happy if they like something that is fun
happy(Person) :- likes(Person, Activity), fun(Activity).

% Two people are friends if they like the same thing
friends(Person1, Person2) :- 
    likes(Person1, Activity),
    likes(Person2, Activity),
    Person1 \= Person2.

%
% Example queries to try:
%
% ?- likes(alice, programming).     % Is Alice a programmer?
% ?- likes(bob, X).                 % What does Bob like?
% ?- happy(alice).                  % Is Alice happy?
% ?- friends(alice, bob).           % Are Alice and Bob friends?
% ?- friends(X, Y).                 % Who are friends?
%
