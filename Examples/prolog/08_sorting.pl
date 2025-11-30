%
% Sorting Algorithms
%
% Demonstrates:
% - Different sorting techniques
% - Algorithm implementation in Prolog
% - Recursive approaches
% - List manipulation
%

% BUBBLE SORT
% Repeatedly swap adjacent elements if in wrong order
bubble_sort(List, Sorted) :- 
    swap(List, List1), 
    !, 
    bubble_sort(List1, Sorted).
bubble_sort(Sorted, Sorted).

swap([X, Y|Rest], [Y, X|Rest]) :- X > Y.
swap([Z|Rest], [Z|Rest1]) :- swap(Rest, Rest1).


% INSERTION SORT  
% Build sorted list by inserting elements one at a time
insertion_sort([], []).
insertion_sort([H|T], Sorted) :-
    insertion_sort(T, SortedT),
    insert_sorted(H, SortedT, Sorted).

insert_sorted(X, [], [X]).
insert_sorted(X, [H|T], [X, H|T]) :- X =< H.
insert_sorted(X, [H|T], [H|Result]) :-
    X > H,
    insert_sorted(X, T, Result).


% SELECTION SORT
% Repeatedly find minimum and add to sorted list
selection_sort([], []).
selection_sort(List, [Min|Sorted]) :-
    select_min(List, Min, Rest),
    selection_sort(Rest, Sorted).

select_min([X], X, []).
select_min([H|T], Min, [H|Rest]) :-
    select_min(T, Min, Rest),
    H > Min.
select_min([H|T], H, [Min|Rest]) :-
    select_min(T, Min, Rest),
    H =< Min.


% MERGE SORT
% Divide list, sort halves, merge results
merge_sort([], []).
merge_sort([X], [X]).
merge_sort(List, Sorted) :-
    List = [_,_|_],
    split(List, Left, Right),
    merge_sort(Left, SortedLeft),
    merge_sort(Right, SortedRight),
    merge(SortedLeft, SortedRight, Sorted).

split([], [], []).
split([X], [X], []).
split([X, Y|Rest], [X|Left], [Y|Right]) :- 
    split(Rest, Left, Right).

merge([], L, L).
merge(L, [], L).
merge([H1|T1], [H2|T2], [H1|Result]) :-
    H1 =< H2,
    merge(T1, [H2|T2], Result).
merge([H1|T1], [H2|T2], [H2|Result]) :-
    H1 > H2,
    merge([H1|T1], T2, Result).


% QUICK SORT
% Partition around pivot, sort partitions
quick_sort([], []).
quick_sort([Pivot|Rest], Sorted) :-
    partition(Pivot, Rest, Less, Greater),
    quick_sort(Less, SortedLess),
    quick_sort(Greater, SortedGreater),
    append(SortedLess, [Pivot|SortedGreater], Sorted).

partition(_, [], [], []).
partition(Pivot, [H|T], [H|Less], Greater) :-
    H =< Pivot,
    partition(Pivot, T, Less, Greater).
partition(Pivot, [H|T], Less, [H|Greater]) :-
    H > Pivot,
    partition(Pivot, T, Less, Greater).

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).


% CHECK IF SORTED
is_sorted([]).
is_sorted([_]).
is_sorted([X, Y|Rest]) :- 
    X =< Y, 
    is_sorted([Y|Rest]).


% BENCHMARK HELPER
% Test sorting algorithm with sample data
test_sort(Algorithm, Input, Sorted, Time) :-
    statistics(runtime, [Start|_]),
    call(Algorithm, Input, Sorted),
    statistics(runtime, [End|_]),
    Time is End - Start.

%
% Example queries:
%
% ?- bubble_sort([5,2,8,1,9], S).           % Bubble sort
% ?- insertion_sort([5,2,8,1,9], S).        % Insertion sort
% ?- selection_sort([5,2,8,1,9], S).        % Selection sort
% ?- merge_sort([5,2,8,1,9], S).            % Merge sort
% ?- quick_sort([5,2,8,1,9], S).            % Quick sort
% ?- is_sorted([1,2,5,8,9]).                % Check if sorted
%
% Compare performance:
% ?- test_sort(bubble_sort, [5,2,8,1,9,3,7,4,6], S, T).
% ?- test_sort(quick_sort, [5,2,8,1,9,3,7,4,6], S, T).
%
