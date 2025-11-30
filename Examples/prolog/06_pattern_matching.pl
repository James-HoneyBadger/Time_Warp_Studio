%
% Pattern Matching and Unification
%
% Demonstrates:
% - Pattern matching with structures
% - Unification
% - Complex data structures
% - Term manipulation
%

% Define structures for people
person(name(First, Last), age(Years), occupation(Job)).

% Sample data
employee(person(name(john, doe), age(30), occupation(engineer))).
employee(person(name(jane, smith), age(28), occupation(designer))).
employee(person(name(bob, jones), age(35), occupation(manager))).

% Extract first name from person
first_name(person(name(First, _), _, _), First).

% Extract age from person
person_age(person(_, age(Years), _), Years).

% Extract occupation from person
person_job(person(_, _, occupation(Job)), Job).

% Define structures for books
book(title(T), author(A), year(Y), pages(P)).

% Library data
in_library(book(title('The Hobbit'), author('Tolkien'), year(1937), pages(310))).
in_library(book(title('1984'), author('Orwell'), year(1949), pages(328))).
in_library(book(title('Dune'), author('Herbert'), year(1965), pages(688))).

% Find books by author
by_author(Author, Title) :-
    in_library(book(title(Title), author(Author), _, _)).

% Find old books (before 1950)
old_book(Title) :-
    in_library(book(title(Title), _, year(Y), _)),
    Y < 1950.

% Define geometric shapes
shape(circle(Radius)).
shape(rectangle(Width, Height)).
shape(triangle(Base, Height)).

% Calculate area based on shape
area(circle(R), Area) :- 
    Area is 3.14159 * R * R.
area(rectangle(W, H), Area) :- 
    Area is W * H.
area(triangle(B, H), Area) :- 
    Area is 0.5 * B * H.

% Tree structures
tree(empty).
tree(node(Value, Left, Right)) :-
    tree(Left),
    tree(Right).

% Count nodes in tree
count_nodes(empty, 0).
count_nodes(node(_, Left, Right), Count) :-
    count_nodes(Left, LeftCount),
    count_nodes(Right, RightCount),
    Count is LeftCount + RightCount + 1.

% Check if value is in tree
in_tree(X, node(X, _, _)).
in_tree(X, node(_, Left, _)) :- in_tree(X, Left).
in_tree(X, node(_, _, Right)) :- in_tree(X, Right).

% Tree depth
tree_depth(empty, 0).
tree_depth(node(_, Left, Right), Depth) :-
    tree_depth(Left, LeftDepth),
    tree_depth(Right, RightDepth),
    max_num(LeftDepth, RightDepth, MaxDepth),
    Depth is MaxDepth + 1.

max_num(X, Y, X) :- X >= Y.
max_num(X, Y, Y) :- Y > X.

% Binary search tree insert
insert(X, empty, node(X, empty, empty)).
insert(X, node(Y, Left, Right), node(Y, NewLeft, Right)) :-
    X < Y,
    insert(X, Left, NewLeft).
insert(X, node(Y, Left, Right), node(Y, Left, NewRight)) :-
    X >= Y,
    insert(X, Right, NewRight).

%
% Example queries:
%
% ?- employee(P), first_name(P, john).      % Find John
% ?- employee(P), person_age(P, A), A > 30. % Employees over 30
% ?- by_author('Tolkien', Title).            % Books by Tolkien
% ?- old_book(T).                            % Old books
% ?- area(circle(5), A).                     % Circle area
% ?- area(rectangle(4, 6), A).               % Rectangle area
% ?- count_nodes(node(1, node(2, empty, empty), node(3, empty, empty)), C).
% ?- tree_depth(node(1, node(2, empty, empty), empty), D).
%
