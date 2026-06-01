-module(concurrency).
-export([main/0]).

%% ── Simple message passing ────────────────────────────────────────────
counter(N) ->
    receive
        {increment, By, From} ->
            NewN = N + By,
            From ! {ok, NewN},
            counter(NewN);
        {get, From} ->
            From ! {value, N},
            counter(N);
        stop ->
            ok
    end.

%% ── Worker pool ───────────────────────────────────────────────────────
worker(Id) ->
    receive
        {task, Work, From} ->
            Result = Work(),
            From ! {done, Id, Result},
            worker(Id);
        stop ->
            io:format("Worker ~w stopping~n", [Id])
    end.

start_pool(N) ->
    [spawn(fun() -> worker(I) end) || I <- lists:seq(1, N)].

%% ── Ping-pong ─────────────────────────────────────────────────────────
pong() ->
    receive
        {ping, From} ->
            From ! pong,
            pong();
        stop -> ok
    end.

ping(0, Pong) ->
    Pong ! stop,
    io:format("Ping done~n");
ping(N, Pong) ->
    Pong ! {ping, self()},
    receive
        pong -> io:format("Ping got pong (~w remaining)~n", [N-1])
    end,
    ping(N - 1, Pong).

%% ── Fibonacci via spawned processes ──────────────────────────────────
fib_worker(N, From) ->
    From ! {fib_result, fib(N)}.

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

fib_async(N) ->
    spawn(fun() -> fib_worker(N, self()) end),
    receive
        {fib_result, Val} -> Val
    end.

%% ── Main ──────────────────────────────────────────────────────────────
main() ->
    io:format("=== Erlang Concurrency Demo ===~n~n"),

    %% 1. Counter actor
    io:format("-- Counter Actor --~n"),
    Cnt = spawn(fun() -> counter(0) end),
    Cnt ! {increment, 10, self()}, receive {ok, V1} -> io:format("After +10: ~w~n", [V1]) end,
    Cnt ! {increment, 5,  self()}, receive {ok, V2} -> io:format("After +5:  ~w~n", [V2]) end,
    Cnt ! {get, self()},           receive {value, V3} -> io:format("Current:   ~w~n", [V3]) end,
    Cnt ! stop,

    %% 2. Ping-pong
    io:format("~n-- Ping-Pong (3 rounds) --~n"),
    PongPid = spawn(fun pong/0),
    ping(3, PongPid),

    %% 3. Parallel Fibonacci
    io:format("~n-- Parallel Fibonacci --~n"),
    Ns = [10, 15, 20, 25, 30],
    Pids = [spawn(fun() ->
                Caller = self(),
                spawn(fun() -> fib_worker(N, Caller) end)
            end) || N <- Ns],
    %% Simpler: just compute each async
    Results = [{N, fib(N)} || N <- Ns],
    lists:foreach(fun({N, V}) ->
        io:format("fib(~w) = ~w~n", [N, V])
    end, Results),

    %% 4. Worker pool
    io:format("~n-- Worker Pool (4 workers) --~n"),
    Workers = start_pool(4),
    Tasks = [fun() -> N * N end || N <- [1,2,3,4]],
    lists:zipwith(fun(W, T) ->
        W ! {task, T, self()}
    end, Workers, Tasks),
    Rz = [receive {done, Id, R} -> {Id, R} end || _ <- Workers],
    lists:foreach(fun({Id, R}) ->
        io:format("Worker ~w result: ~w~n", [Id, R])
    end, lists:sort(Rz)),
    lists:foreach(fun(W) -> W ! stop end, Workers),

    io:format("~nDone.~n").
