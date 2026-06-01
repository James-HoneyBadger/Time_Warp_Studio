% Prolog Sorting Algorithms
% Quicksort, merge sort, insertion sort, and bubble sort

% ── Quicksort ──────────────────────────────────────────────────────────
partition(_, [], [], []).
partition(Pivot, [H|T], [H|Less], Greater) :-
    H =< Pivot, !,
    partition(Pivot, T, Less, Greater).
partition(Pivot, [H|T], Less, [H|Greater]) :-
    partition(Pivot, T, Less, Greater).

quicksort([], []).
quicksort([H|T], Sorted) :-
    partition(H, T, Less, Greater),
    quicksort(Less, SortedLess),
    quicksort(Greater, SortedGreater),
    append(SortedLess, [H|SortedGreater], Sorted).

% ── Merge sort ─────────────────────────────────────────────────────────
split([], [], []).
split([X], [X], []).
split([X,Y|T], [X|L], [Y|R]) :- split(T, L, R).

merge([], R, R).
merge(L, [], L).
merge([H1|T1], [H2|T2], [H1|Merged]) :-
    H1 =< H2, !,
    merge(T1, [H2|T2], Merged).
merge([H1|T1], [H2|T2], [H2|Merged]) :-
    merge([H1|T1], T2, Merged).

mergesort([], []).
mergesort([X], [X]).
mergesort(List, Sorted) :-
    List = [_,_|_],
    split(List, L, R),
    mergesort(L, SL),
    mergesort(R, SR),
    merge(SL, SR, Sorted).

% ── Insertion sort ─────────────────────────────────────────────────────
insert_sorted(X, [], [X]).
insert_sorted(X, [H|T], [X,H|T]) :- X =< H, !.
insert_sorted(X, [H|T], [H|R]) :- insert_sorted(X, T, R).

insertion_sort([], []).
insertion_sort([H|T], Sorted) :-
    insertion_sort(T, ST),
    insert_sorted(H, ST, Sorted).

% ── Bubble sort ────────────────────────────────────────────────────────
bubble_pass([], [], false).
bubble_pass([X], [X], false).
bubble_pass([X,Y|T], [X|R], Swapped) :-
    X =< Y, !,
    bubble_pass([Y|T], R, Swapped).
bubble_pass([X,Y|T], [Y|R], true) :-
    bubble_pass([X|T], R, _).

bubble_sort(L, Sorted) :- bubble_sort_(L, Sorted, false).
bubble_sort_(L, L, false) :- !.  % no swaps → already sorted
bubble_sort_(L, Sorted, _) :-
    bubble_pass(L, L1, _),
    bubble_sort(L1, Sorted).

% ── Demos ──────────────────────────────────────────────────────────────
:- initialization(main).

main :-
    writeln('=== Prolog Sorting Algorithms ==='),
    Data = [64, 25, 12, 90, 3, 77, 44, 18, 55, 37],
    write('Input: '), writeln(Data),
    nl,

    quicksort(Data, QS),
    write('Quicksort:      '), writeln(QS),

    mergesort(Data, MS),
    write('Merge sort:     '), writeln(MS),

    insertion_sort(Data, IS),
    write('Insertion sort: '), writeln(IS),

    bubble_sort(Data, BS),
    write('Bubble sort:    '), writeln(BS),

    nl,
    writeln('=== Sorted string list ==='),
    Words = [banana, apple, cherry, date, elderberry],
    write('Input: '), writeln(Words),
    quicksort(Words, SortedWords),
    write('Sorted: '), writeln(SortedWords).
