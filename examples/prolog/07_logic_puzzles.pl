%
% Logic Puzzles
%
% Demonstrates:
% - Constraint solving
% - Logical deduction
% - Problem-solving techniques
% - Generate and test approach
%

% PUZZLE 1: Einstein's Riddle (simplified)
% Who owns the fish?
%
% Houses: 5 houses in a row
% Each house has a different color, nationality, pet, drink, and smoke
%
einstein_puzzle(FishOwner) :-
    % Define houses as list of house(Color, Nationality, Pet, Drink, Smoke)
    Houses = [_, _, _, _, _],
    
    % The British lives in the red house
    member(house(red, british, _, _, _), Houses),
    
    % The Swedish keeps dogs
    member(house(_, swedish, dogs, _, _), Houses),
    
    % The Danish drinks tea
    member(house(_, danish, _, tea, _), Houses),
    
    % The green house is on the left of the white house
    next_to(house(green, _, _, _, _), house(white, _, _, _, _), Houses),
    
    % The person in the green house drinks coffee
    member(house(green, _, _, coffee, _), Houses),
    
    % The person who smokes Pall Mall keeps birds
    member(house(_, _, birds, _, pallmall), Houses),
    
    % The person in the yellow house smokes Dunhill
    member(house(yellow, _, _, _, dunhill), Houses),
    
    % The person in the center house drinks milk
    Houses = [_, _, house(_, _, _, milk, _), _, _],
    
    % The Norwegian lives in the first house
    Houses = [house(_, norwegian, _, _, _)|_],
    
    % The person who smokes Blend lives next to the person with cats
    next_to(house(_, _, _, _, blend), house(_, _, cats, _, _), Houses),
    
    % The person who keeps horses lives next to the person who smokes Dunhill
    next_to(house(_, _, horses, _, _), house(_, _, _, _, dunhill), Houses),
    
    % The person who smokes Blue Master drinks beer
    member(house(_, _, _, beer, bluemaster), Houses),
    
    % The German smokes Prince
    member(house(_, german, _, _, prince), Houses),
    
    % The Norwegian lives next to the blue house
    next_to(house(_, norwegian, _, _, _), house(blue, _, _, _, _), Houses),
    
    % The person who smokes Blend has a neighbor who drinks water
    next_to(house(_, _, _, _, blend), house(_, _, _, water, _), Houses),
    
    % Who owns the fish?
    member(house(_, FishOwner, fish, _, _), Houses).

% Helper: member predicate
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% Helper: next_to predicate
next_to(X, Y, List) :- append(_, [X, Y|_], List).
next_to(X, Y, List) :- append(_, [Y, X|_], List).

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).


% PUZZLE 2: Missionaries and Cannibals
% 3 missionaries and 3 cannibals need to cross a river
% Boat holds max 2 people
% Missionaries can never be outnumbered by cannibals on either side
%
safe_state(M, C) :- 
    M >= 0, C >= 0, M =< 3, C =< 3,
    (M = 0 ; M >= C),
    M2 is 3 - M, C2 is 3 - C,
    (M2 = 0 ; M2 >= C2).

% PUZZLE 3: Sudoku cell (simplified)
% Fill a 3x3 grid with numbers 1-9, no repeats in row/column
%
sudoku_cell(Grid) :-
    Grid = [[A1,A2,A3],
            [B1,B2,B3],
            [C1,C2,C3]],
    
    % All different in rows
    all_different([A1,A2,A3]),
    all_different([B1,B2,B3]),
    all_different([C1,C2,C3]),
    
    % All different in columns
    all_different([A1,B1,C1]),
    all_different([A2,B2,C2]),
    all_different([A3,B3,C3]),
    
    % Numbers 1-9
    valid_nums([A1,A2,A3,B1,B2,B3,C1,C2,C3]).

all_different([]).
all_different([H|T]) :- \+ member(H, T), all_different(T).

valid_nums([]).
valid_nums([H|T]) :- between(1, 9, H), valid_nums(T).

between(Low, High, Low) :- Low =< High.
between(Low, High, X) :- 
    Low < High,
    Low1 is Low + 1,
    between(Low1, High, X).


% PUZZLE 4: Wolf, Goat, Cabbage
% Farmer must transport wolf, goat, and cabbage across river
% Can only take one at a time
% Wolf eats goat if left alone
% Goat eats cabbage if left alone
%
safe_arrangement(wolf, goat, _) :- !, fail.
safe_arrangement(_, goat, cabbage) :- !, fail.
safe_arrangement(_, _, _).

%
% Example queries:
%
% ?- einstein_puzzle(Owner).        % Who owns the fish?
% ?- safe_state(3, 2).              % Is this state safe?
% ?- sudoku_cell(G).                % Generate valid grid
%
