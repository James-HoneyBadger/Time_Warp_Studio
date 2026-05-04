# Erlang Algorithms & Data Structures
# Time Warp Studio - Classic algorithms in functional style

-module(algorithms).
-export([main/0]).

% ── Sorting ───────────────────────────────────────────────────────────────────

quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
    Less    = [X || X <- Rest, X =< Pivot],
    Greater = [X || X <- Rest, X > Pivot],
    quicksort(Less) ++ [Pivot] ++ quicksort(Greater).

mergesort([])  -> [];
mergesort([X]) -> [X];
mergesort(List) ->
    Mid = length(List) div 2,
    {Left, Right} = lists:split(Mid, List),
    merge(mergesort(Left), mergesort(Right)).

merge([], R) -> R;
merge(L, []) -> L;
merge([H1|T1], [H2|T2]) when H1 =< H2 -> [H1 | merge(T1, [H2|T2])];
merge([H1|T1], [H2|T2])               -> [H2 | merge([H1|T1], T2)].

% ── Searching ─────────────────────────────────────────────────────────────────

binary_search(List, Target) ->
    binary_search(List, Target, 0, length(List) - 1).
binary_search(_, _, Low, High) when Low > High -> not_found;
binary_search(List, Target, Low, High) ->
    Mid = (Low + High) div 2,
    Val = lists:nth(Mid + 1, List),
    if
        Val =:= Target -> {found, Mid};
        Val < Target   -> binary_search(List, Target, Mid + 1, High);
        true           -> binary_search(List, Target, Low, Mid - 1)
    end.

% ── Map / Dict operations ─────────────────────────────────────────────────────

% Word frequency count using a map
word_count(Words) ->
    lists:foldl(
        fun(W, Map) ->
            Count = maps:get(W, Map, 0),
            maps:put(W, Count + 1, Map)
        end,
        #{},
        Words
    ).

% ── Graph traversal (adjacency list) ─────────────────────────────────────────

% BFS using a queue (list as queue)
bfs(Graph, Start) ->
    bfs(Graph, [Start], [Start], []).
bfs(_, [], Visited, _) -> lists:reverse(Visited);
bfs(Graph, [H|Queue], Visited, _) ->
    Neighbors = maps:get(H, Graph, []),
    NewNeighbors = [N || N <- Neighbors, not lists:member(N, Visited)],
    bfs(Graph, Queue ++ NewNeighbors, Visited ++ NewNeighbors, []).

% ── Prime sieve ───────────────────────────────────────────────────────────────

sieve(Max) -> sieve(lists:seq(2, Max), []).
sieve([], Primes) -> lists:reverse(Primes);
sieve([H|T], Primes) ->
    Filtered = [X || X <- T, X rem H =/= 0],
    sieve(Filtered, [H | Primes]).

% ── Main ──────────────────────────────────────────────────────────────────────

main() ->
    io:format("=== Erlang Algorithms & Data Structures ===~n~n"),

    %% Sorting
    io:format("Sorting:~n"),
    Unsorted = [38, 27, 43, 3, 9, 82, 10, 1, 55, 17],
    io:format("  Input:     ~w~n", [Unsorted]),
    io:format("  Quicksort: ~w~n", [quicksort(Unsorted)]),
    io:format("  Mergesort: ~w~n", [mergesort(Unsorted)]),

    %% Binary search
    io:format("~nBinary Search:~n"),
    Sorted = lists:sort(Unsorted),
    io:format("  Sorted: ~w~n", [Sorted]),
    lists:foreach(fun(T) ->
        Res = binary_search(Sorted, T),
        io:format("  search(~w) -> ~w~n", [T, Res])
    end, [3, 55, 42]),

    %% Word frequency
    io:format("~nWord Frequency:~n"),
    Words = [the, cat, sat, on, the, mat, the, cat, sat],
    Freq = word_count(Words),
    maps:foreach(fun(W, C) ->
        io:format("  ~w: ~w~n", [W, C])
    end, Freq),

    %% BFS graph traversal
    io:format("~nBFS Graph Traversal:~n"),
    Graph = #{
        a => [b, c],
        b => [a, d, e],
        c => [a, f],
        d => [b],
        e => [b, f],
        f => [c, e]
    },
    Order = bfs(Graph, a),
    io:format("  BFS from 'a': ~w~n", [Order]),

    %% Prime sieve
    io:format("~nPrimes up to 50 (Sieve of Eratosthenes):~n"),
    Primes = sieve(50),
    io:format("  ~w~n", [Primes]),
    io:format("  Count: ~w~n", [length(Primes)]),

    io:format("~nDone.~n").
