%% ============================================================
%% NUMBER THEORY — Erlang Language Showcase
%% Primes, GCD/LCM, totient, perfect numbers, modular arithmetic
%% Time Warp Studio — Erlang Language Demo
%% ============================================================

-module(number_theory).
-export([main/0]).

%% ============================================================
%% PRIME NUMBERS
%% ============================================================

%% Trial division primality test
is_prime(2) -> true;
is_prime(3) -> true;
is_prime(N) when N < 2 -> false;
is_prime(N) when N rem 2 =:= 0 -> false;
is_prime(N) -> is_prime_trial(N, 3).

is_prime_trial(N, F) when F * F > N -> true;
is_prime_trial(N, F) when N rem F =:= 0 -> false;
is_prime_trial(N, F) -> is_prime_trial(N, F + 2).

%% Sieve of Eratosthenes up to N
sieve(N) ->
    Candidates = lists:seq(2, N),
    sieve_filter(Candidates).

sieve_filter([]) -> [];
sieve_filter([P | Rest]) ->
    [P | sieve_filter([X || X <- Rest, X rem P =/= 0])].

%% Prime factorization
prime_factors(1) -> [];
prime_factors(N) -> prime_factors(N, 2, []).

prime_factors(1, _, Acc) -> lists:reverse(Acc);
prime_factors(N, F, Acc) when F * F > N ->
    lists:reverse([N | Acc]);
prime_factors(N, F, Acc) when N rem F =:= 0 ->
    prime_factors(N div F, F, [F | Acc]);
prime_factors(N, F, Acc) ->
    prime_factors(N, F + 1, Acc).

%% ============================================================
%% GCD AND LCM
%% ============================================================

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

lcm(A, B) -> (A * B) div gcd(A, B).

%% Extended Euclidean algorithm: returns {GCD, X, Y} where aX + bY = GCD
extended_gcd(A, 0) -> {A, 1, 0};
extended_gcd(A, B) ->
    {G, X1, Y1} = extended_gcd(B, A rem B),
    {G, Y1, X1 - (A div B) * Y1}.

%% ============================================================
%% EULER'S TOTIENT FUNCTION
%% ============================================================

totient(1) -> 1;
totient(N) ->
    Factors = lists:usort(prime_factors(N)),
    lists:foldl(
        fun(P, Acc) -> Acc * (P - 1) div P end,
        N, Factors).

%% ============================================================
%% PERFECT NUMBERS AND DIVISOR FUNCTIONS
%% ============================================================

%% Sum of proper divisors
sum_proper_divisors(N) ->
    lists:sum([D || D <- lists:seq(1, N div 2), N rem D =:= 0]).

is_perfect(N) -> sum_proper_divisors(N) =:= N.
is_abundant(N) -> sum_proper_divisors(N) > N.
is_deficient(N) -> sum_proper_divisors(N) < N.

%% Amicable pair: a and b are amicable if sigma(a)=b and sigma(b)=a
are_amicable(A, B) when A =/= B ->
    sum_proper_divisors(A) =:= B andalso sum_proper_divisors(B) =:= A;
are_amicable(_, _) -> false.

%% Find amicable pairs up to N
find_amicable_pairs(N) ->
    [{A, B} || A <- lists:seq(2, N),
               B <- [sum_proper_divisors(A)],
               B > A, B =< N,
               are_amicable(A, B)].

%% ============================================================
%% MODULAR ARITHMETIC
%% ============================================================

%% Modular exponentiation: base^exp mod m
mod_pow(_, 0, _) -> 1;
mod_pow(Base, Exp, Mod) when Exp rem 2 =:= 0 ->
    Half = mod_pow(Base, Exp div 2, Mod),
    (Half * Half) rem Mod;
mod_pow(Base, Exp, Mod) ->
    (Base * mod_pow(Base, Exp - 1, Mod)) rem Mod.

%% Modular inverse using extended GCD
mod_inverse(A, M) ->
    {G, X, _} = extended_gcd(A rem M, M),
    case G of
        1 -> ((X rem M) + M) rem M;
        _ -> no_inverse
    end.

%% Chinese Remainder Theorem
%% Given list of {remainder, modulus} pairs, find x
crt(Congruences) ->
    M = lists:foldl(fun({_, Mi}, Acc) -> Acc * Mi end, 1, Congruences),
    X = lists:foldl(
        fun({Ri, Mi}, Acc) ->
            Mi_bar = M div Mi,
            Yi = mod_inverse(Mi_bar, Mi),
            Acc + Ri * Mi_bar * Yi
        end, 0, Congruences),
    X rem M.

%% ============================================================
%% NUMBER SEQUENCES
%% ============================================================

%% Collatz sequence length
collatz_length(1) -> 1;
collatz_length(N) when N rem 2 =:= 0 ->
    1 + collatz_length(N div 2);
collatz_length(N) ->
    1 + collatz_length(3 * N + 1).

%% Collatz sequence as list
collatz_seq(1) -> [1];
collatz_seq(N) when N rem 2 =:= 0 ->
    [N | collatz_seq(N div 2)];
collatz_seq(N) ->
    [N | collatz_seq(3 * N + 1)].

%% Catalan number C(n) = (2n)! / (n+1)! / n!
factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

catalan(N) -> factorial(2*N) div (factorial(N+1) * factorial(N)).

%% ============================================================
%% DISPLAY HELPERS
%% ============================================================

print_bar(Value, Max, Width) ->
    Len = (Value * Width) div max(Max, 1),
    lists:duplicate(Len, $█) ++ lists:duplicate(Width - Len, $ ).

print_factors(N) ->
    F = prime_factors(N),
    io:format("    ~4w = ~s~n", [N,
        string:join([integer_to_list(X) || X <- F], " × ")]).

%% ============================================================
%% MAIN
%% ============================================================

main() ->
    io:format("============================================================~n"),
    io:format("  NUMBER THEORY — Erlang Showcase~n"),
    io:format("  Primes, GCD/LCM, Totient, Perfect nums, Mod arithmetic~n"),
    io:format("============================================================~n~n"),

    %% --- Section 1: Primes ---
    io:format("SECTION 1: PRIME NUMBERS~n"),
    io:format("------------------------------------------------------------~n"),
    Primes100 = sieve(100),
    io:format("  Primes up to 100 (~w total):~n", [length(Primes100)]),
    io:format("    ~w~n~n", [Primes100]),

    io:format("  Primality spot checks:~n"),
    Tests = [2, 7, 11, 13, 17, 97, 100, 101, 997, 1000, 7919],
    lists:foreach(fun(N) ->
        io:format("    ~5w: ~s~n", [N, case is_prime(N) of true -> "prime"; false -> "composite" end])
    end, Tests),
    io:format("~n"),

    %% --- Section 2: Prime factorization ---
    io:format("SECTION 2: PRIME FACTORIZATION~n"),
    io:format("------------------------------------------------------------~n"),
    Nums = [12, 36, 100, 360, 1024, 2310, 9999, 12345, 65536],
    lists:foreach(fun print_factors/1, Nums),
    io:format("~n"),

    %% --- Section 3: GCD / LCM / Extended GCD ---
    io:format("SECTION 3: GCD, LCM, EXTENDED EUCLIDEAN~n"),
    io:format("------------------------------------------------------------~n"),
    Pairs = [{48, 18}, {100, 75}, {56, 98}, {1071, 462}, {270, 192}],
    lists:foreach(fun({A, B}) ->
        G = gcd(A, B),
        L = lcm(A, B),
        {_, X, Y} = extended_gcd(A, B),
        io:format("    gcd(~w, ~w) = ~w   lcm = ~w   ~w*~w + ~w*~w = ~w~n",
            [A, B, G, L, A, X, B, Y, A*X + B*Y])
    end, Pairs),
    io:format("~n"),

    %% --- Section 4: Euler's Totient ---
    io:format("SECTION 4: EULER'S TOTIENT FUNCTION φ(n)~n"),
    io:format("------------------------------------------------------------~n"),
    io:format("  φ(n) = count of integers in [1..n] coprime to n~n~n"),
    io:format("  ~5s  ~5s  ~8s~n", ["n", "φ(n)", "bar"]),
    io:format("  ~5s  ~5s  ~8s~n", ["-----", "-----", "--------"]),
    Totients = lists:seq(1, 20),
    lists:foreach(fun(N) ->
        T = totient(N),
        Bar = print_bar(T, N, 20),
        io:format("  ~5w  ~5w  ~s~n", [N, T, Bar])
    end, Totients),
    io:format("~n"),

    %% --- Section 5: Perfect, abundant, deficient ---
    io:format("SECTION 5: PERFECT, ABUNDANT, DEFICIENT NUMBERS~n"),
    io:format("------------------------------------------------------------~n"),
    Range = lists:seq(1, 50),
    Perfect   = [N || N <- Range, is_perfect(N)],
    Abundant  = [N || N <- Range, is_abundant(N)],
    Deficient = [N || N <- Range, is_deficient(N)],
    io:format("  Perfect  (sigma(n)=2n):   ~w~n", [Perfect]),
    io:format("  Abundant (sigma(n)>2n):   ~w~n", [Abundant]),
    io:format("  Deficient(sigma(n)<2n):   ~w~n~n", [Deficient]),

    %% --- Section 6: Amicable pairs ---
    io:format("SECTION 6: AMICABLE PAIRS (up to 2000)~n"),
    io:format("------------------------------------------------------------~n"),
    Amicable = find_amicable_pairs(2000),
    case Amicable of
        [] -> io:format("  None found up to 2000~n~n");
        _ ->
            lists:foreach(fun({A, B}) ->
                io:format("  (~w, ~w): σ(~w)=~w, σ(~w)=~w~n",
                    [A, B, A, sum_proper_divisors(A), B, sum_proper_divisors(B)])
            end, Amicable),
            io:format("~n")
    end,

    %% --- Section 7: Modular arithmetic ---
    io:format("SECTION 7: MODULAR ARITHMETIC~n"),
    io:format("------------------------------------------------------------~n"),
    io:format("  Modular exponentiation (base^exp mod m):~n"),
    ModTests = [{2,10,1000}, {3,200,13}, {7,50,100}, {17,13,31}],
    lists:foreach(fun({B,E,M}) ->
        io:format("    ~w^~w mod ~w = ~w~n", [B, E, M, mod_pow(B, E, M)])
    end, ModTests),
    io:format("~n"),
    io:format("  Modular inverses:~n"),
    InvTests = [{3,7}, {5,11}, {7,26}, {10,17}],
    lists:foreach(fun({A,M}) ->
        case mod_inverse(A, M) of
            no_inverse -> io:format("    ~w^-1 mod ~w = none (not coprime)~n", [A, M]);
            Inv ->
                io:format("    ~w^-1 mod ~w = ~w  (check: ~w*~w mod ~w = ~w)~n",
                    [A, M, Inv, A, Inv, M, (A * Inv) rem M])
        end
    end, InvTests),
    io:format("~n"),

    %% --- Section 8: Chinese Remainder Theorem ---
    io:format("SECTION 8: CHINESE REMAINDER THEOREM~n"),
    io:format("------------------------------------------------------------~n"),
    io:format("  Find x such that x ≡ r (mod m) for each pair:~n~n"),
    CRTProblems = [
        [{2,3},{3,5},{2,7}],
        [{1,2},{2,3},{3,5}],
        [{0,3},{3,4},{4,5}]
    ],
    lists:foreach(fun(Congruences) ->
        Desc = string:join(
            ["x≡" ++ integer_to_list(R) ++ "(mod" ++ integer_to_list(M) ++ ")"
             || {R,M} <- Congruences], ", "),
        X = crt(Congruences),
        io:format("  ~s  =>  x = ~w~n", [Desc, X]),
        lists:foreach(fun({R,M}) ->
            io:format("    verify: ~w mod ~w = ~w ~s~n",
                [X, M, X rem M, case X rem M =:= R of true -> "✓"; false -> "✗" end])
        end, Congruences),
        io:format("~n")
    end, CRTProblems),

    %% --- Section 9: Collatz conjecture ---
    io:format("SECTION 9: COLLATZ CONJECTURE~n"),
    io:format("------------------------------------------------------------~n"),
    io:format("  For any n: if even, n/2; if odd, 3n+1. Always reaches 1.~n~n"),
    Starts = [6, 11, 27, 97, 231],
    lists:foreach(fun(N) ->
        Seq = collatz_seq(N),
        Len = length(Seq),
        Max = lists:max(Seq),
        io:format("  Start=~3w: length=~3w  max=~6w  first10=~w~n",
            [N, Len, Max, lists:sublist(Seq, 10)])
    end, Starts),
    io:format("~n"),
    io:format("  Longest Collatz sequence starting under 100:~n"),
    {MaxN, MaxLen} = lists:foldl(
        fun(N, {BestN, BestLen}) ->
            L = collatz_length(N),
            if L > BestLen -> {N, L}; true -> {BestN, BestLen} end
        end, {1, 1}, lists:seq(1, 99)),
    io:format("    n=~w, length=~w~n~n", [MaxN, MaxLen]),

    %% --- Section 10: Catalan numbers ---
    io:format("SECTION 10: CATALAN NUMBERS~n"),
    io:format("------------------------------------------------------------~n"),
    io:format("  C(n) = (2n)! / (n+1)! / n!  (parenthesization, tree counts, Dyck paths)~n~n"),
    io:format("  ~4s  ~10s~n", ["n", "C(n)"]),
    io:format("  ~4s  ~10s~n", ["----", "----------"]),
    lists:foreach(fun(N) ->
        io:format("  ~4w  ~10w~n", [N, catalan(N)])
    end, lists:seq(0, 12)),
    io:format("~n"),

    io:format("============================================================~n"),
    io:format("  Number Theory complete!~n"),
    io:format("  Primes * GCD/LCM * Totient * Perfect * Modular * CRT~n"),
    io:format("============================================================~n").
