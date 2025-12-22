% Simple Knowledge Base Demo

% Facts
likes(mary, food).
likes(mary, wine).
likes(john, wine).
likes(john, mary).

% Rules
friends(X, Y) :- likes(X, Z), likes(Y, Z).

% Query example:
% ?- friends(john, mary).
