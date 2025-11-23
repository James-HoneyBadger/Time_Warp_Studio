%
% List Operations in Prolog
%
% Demonstrates:
% - List syntax and notation
% - Head and tail decomposition
% - Recursive list processing
% - Common list predicates
%

% Check if element is member of list
member_of(X, [X|_]).
member_of(X, [_|Tail]) :- member_of(X, Tail).

% Length of a list
list_length([], 0).
list_length([_|Tail], N) :- 
    list_length(Tail, N1), 
    N is N1 + 1.

% Sum of list elements
sum_list([], 0).
sum_list([Head|Tail], Sum) :-
    sum_list(Tail, RestSum),
    Sum is Head + RestSum.

% Maximum element in list
max_list([X], X).
max_list([Head|Tail], Max) :-
    max_list(Tail, TailMax),
    (Head > TailMax -> Max = Head ; Max = TailMax).

% Append two lists
append_lists([], L, L).
append_lists([H|T1], L2, [H|Result]) :- 
    append_lists(T1, L2, Result).

% Reverse a list
reverse_list([], []).
reverse_list([H|T], Rev) :-
    reverse_list(T, RevT),
    append_lists(RevT, [H], Rev).

% Last element of list
last_element([X], X).
last_element([_|Tail], Last) :- last_element(Tail, Last).

% First N elements of list
take(0, _, []).
take(N, [H|T], [H|Result]) :-
    N > 0,
    N1 is N - 1,
    take(N1, T, Result).

% Remove element from list
remove_element(_, [], []).
remove_element(X, [X|Tail], Result) :- 
    remove_element(X, Tail, Result).
remove_element(X, [H|Tail], [H|Result]) :- 
    X \= H,
    remove_element(X, Tail, Result).

% Check if list is sorted (ascending)
is_sorted([]).
is_sorted([_]).
is_sorted([X, Y|Tail]) :- 
    X =< Y, 
    is_sorted([Y|Tail]).

%
% Example queries:
%
% ?- member_of(3, [1,2,3,4]).       % Is 3 in the list?
% ?- list_length([1,2,3,4,5], N).   % What's the length?
% ?- sum_list([1,2,3,4,5], Sum).    % Sum of elements?
% ?- max_list([3,7,2,9,1], Max).    % Maximum element?
% ?- append_lists([1,2], [3,4], X). % Concatenate lists
% ?- reverse_list([1,2,3,4], R).    % Reverse the list
% ?- is_sorted([1,2,3,4]).          % Is it sorted?
%
