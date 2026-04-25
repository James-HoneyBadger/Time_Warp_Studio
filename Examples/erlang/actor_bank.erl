-module(actor_bank).
-export([start/0]).

%% ============================================================
%% ACTOR BANK — Erlang Concurrent Programming Showcase
%% Account State Machine * Pattern Matching * Actor Model
%% Time Warp Studio — Erlang Language Demo
%% ============================================================

%% ===== ACCOUNT RECORD =====
%% Erlang uses tagged tuples as records

new_account(Name, Balance) ->
    {account, Name, Balance, [], 1}.

account_name({account, Name, _, _, _}) -> Name.
account_balance({account, _, Balance, _, _}) -> Balance.
account_transactions({account, _, _, Txns, _}) -> Txns.
account_id({account, _, _, _, Id}) -> Id.

%% Process a transaction, return {ok, NewAccount} | {error, Reason}
process({account, Name, Bal, Txns, Id}, {deposit, Amount}) when Amount > 0 ->
    NewBal = Bal + Amount,
    Txn = {Id, deposit, Amount, NewBal},
    {ok, {account, Name, NewBal, [Txn | Txns], Id + 1}};

process({account, Name, Bal, Txns, Id}, {withdraw, Amount}) when Amount > 0, Amount =< Bal ->
    NewBal = Bal - Amount,
    Txn = {Id, withdraw, Amount, NewBal},
    {ok, {account, Name, NewBal, [Txn | Txns], Id + 1}};

process({account, _, Bal, _, _}, {withdraw, Amount}) when Amount > Bal ->
    {error, insufficient_funds};

process(_, {withdraw, Amount}) when Amount =< 0 ->
    {error, invalid_amount};

process({account, Name, Bal, Txns, Id}, {transfer_in, Amount, From}) ->
    NewBal = Bal + Amount,
    Txn = {Id, transfer_in, Amount, NewBal, From},
    {ok, {account, Name, NewBal, [Txn | Txns], Id + 1}};

process(Account, {balance_inquiry}) ->
    {ok, account_balance(Account), Account};

process(_, _) ->
    {error, unknown_operation}.

%% ===== BANK OPERATIONS =====

apply_transactions(Account, []) ->
    {ok, Account};
apply_transactions(Account, [Op | Rest]) ->
    case process(Account, Op) of
        {ok, NewAccount} ->
            apply_transactions(NewAccount, Rest);
        {ok, _Balance, NewAccount} ->
            apply_transactions(NewAccount, Rest);
        {error, Reason} ->
            io:format("    ❌ Transaction failed: ~w~n", [Reason]),
            apply_transactions(Account, Rest)
    end.

%% ===== INTEREST CALCULATION =====

compound_interest(Principal, Rate, Years) ->
    Principal * math:pow(1 + Rate, Years).

monthly_payment(Principal, AnnualRate, Months) ->
    R = AnnualRate / 12,
    P = Principal,
    P * R * math:pow(1 + R, Months) / (math:pow(1 + R, Months) - 1).

%% ===== STATISTICS =====

mean([]) -> 0.0;
mean(List) ->
    lists:sum(List) / length(List).

variance(List) ->
    M = mean(List),
    Diffs = lists:map(fun(X) -> (X - M) * (X - M) end, List),
    mean(Diffs).

std_dev(List) -> math:sqrt(variance(List)).

median(List) ->
    Sorted = lists:sort(List),
    N = length(Sorted),
    Mid = N div 2,
    case N rem 2 of
        0 -> (lists:nth(Mid, Sorted) + lists:nth(Mid + 1, Sorted)) / 2;
        1 -> lists:nth(Mid + 1, Sorted)
    end.

%% ===== LIST PROCESSING =====

%% Running total of transactions
running_totals([], _) -> [];
running_totals([H | T], Acc) ->
    New = Acc + H,
    [New | running_totals(T, New)].

%% Detect fraudulent transactions (outliers > 3 stddev from mean)
flag_suspicious(Transactions, Threshold) ->
    Amounts = lists:map(fun({_, _, Amt, _}) -> Amt;
                           ({_, _, Amt, _, _}) -> Amt end, Transactions),
    M = mean(Amounts),
    S = std_dev(Amounts),
    lists:filter(fun({_, _, Amt, _}) -> abs(Amt - M) > Threshold * S;
                    ({_, _, Amt, _, _}) -> abs(Amt - M) > Threshold * S end,
                 Transactions).

%% ===== PATTERN MATCHING SHOWCASE =====

classify_balance(0) -> bankrupt;
classify_balance(B) when B < 0    -> overdrawn;
classify_balance(B) when B < 100  -> low;
classify_balance(B) when B < 1000 -> medium;
classify_balance(B) when B < 10000 -> comfortable;
classify_balance(_) -> wealthy.

describe_account({account, Name, Balance, Txns, _}) ->
    Class = classify_balance(Balance),
    io:format("    ~s: $~.2f (~w) | ~w transactions~n",
              [Name, float(Balance), Class, length(Txns)]).

%% ===== SIMULATED ACTOR MESSAGES =====

simulate_actor_model() ->
    io:format("  Simulating actor message passing:~n"),
    %% In real Erlang, each account would be a process
    %% Here we simulate the message semantics
    Messages = [
        {alice, {deposit, 500}},
        {bob,   {deposit, 1200}},
        {alice, {withdraw, 200}},
        {bob,   {withdraw, 300}},
        {alice, {transfer_in, 100, bob}},
        {bob,   {withdraw, 2000}},    %% will fail
        {alice, {deposit, 50}},
        {bob,   {deposit, 100}}
    ],
    Accounts = #{
        alice => new_account("Alice Smith", 1000),
        bob   => new_account("Bob Jones",   2500)
    },
    process_messages(Messages, Accounts).

process_messages([], Accounts) ->
    Accounts;
process_messages([{Owner, Op} | Rest], Accounts) ->
    Account = maps:get(Owner, Accounts),
    io:format("    → ~w ! ~w~n", [Owner, Op]),
    case process(Account, Op) of
        {ok, NewAccount} ->
            process_messages(Rest, maps:put(Owner, NewAccount, Accounts));
        {ok, _, NewAccount} ->
            process_messages(Rest, maps:put(Owner, NewAccount, Accounts));
        {error, Reason} ->
            io:format("      ❌ ~w: ~w~n", [Owner, Reason]),
            process_messages(Rest, Accounts)
    end.

%% ===== PRIME SIEVE IN ERLANG STYLE =====

sieve(Max) ->
    All = lists:seq(2, Max),
    sieve_loop(All, []).

sieve_loop([], Primes) -> lists:reverse(Primes);
sieve_loop([P | Rest], Primes) ->
    Filtered = lists:filter(fun(X) -> X rem P /= 0 end, Rest),
    sieve_loop(Filtered, [P | Primes]).

%% ===== MAIN ENTRY =====

start() ->
    io:format("~n============================================================~n"),
    io:format("  ACTOR BANK — Erlang Concurrent Computing Showcase~n"),
    io:format("  Pattern Matching | Recursion | Higher-Order Functions~n"),
    io:format("============================================================~n~n"),

    %% ---- Section 1: Account Operations ----
    io:format("[ 1 ] BANK ACCOUNT STATE MACHINE~n~n"),

    Alice = new_account("Alice Smith", 1000),
    Bob   = new_account("Bob Jones",    500),
    Carol = new_account("Carol White",    0),

    Operations = [
        {deposit, 2000},
        {withdraw, 500},
        {deposit, 100},
        {withdraw, 50},
        {withdraw, 10000}  %% should fail
    ],

    {ok, Alice2} = apply_transactions(Alice, Operations),
    io:format("  Alice after transactions:~n"),
    describe_account(Alice2),
    describe_account(Bob),
    describe_account(Carol),

    %% ---- Section 2: Actor Model Simulation ----
    io:format("~n[ 2 ] ACTOR MODEL — Simulated Message Passing~n~n"),
    FinalAccounts = simulate_actor_model(),
    io:format("~n  Final account states:~n"),
    maps:fold(fun(_, Acc, _) -> describe_account(Acc) end, ok, FinalAccounts),

    %% ---- Section 3: Financial Calculations ----
    io:format("~n[ 3 ] FINANCIAL MATHEMATICS~n~n"),

    io:format("  Compound Interest (5%% APR):~n"),
    lists:foreach(fun(Y) ->
        Final = compound_interest(10000, 0.05, Y),
        io:format("    $10,000 after ~w years = $~.2f~n", [Y, Final])
    end, [1, 5, 10, 20, 30, 40]),

    io:format("~n  Mortgage (30-year, $300,000 at 4.5%%):~n"),
    Payment = monthly_payment(300000, 0.045, 360),
    io:format("    Monthly payment: $~.2f~n", [Payment]),
    io:format("    Total paid: $~.2f~n", [Payment * 360]),
    io:format("    Total interest: $~.2f~n", [Payment * 360 - 300000]),

    %% ---- Section 4: Statistics ----
    io:format("~n[ 4 ] TRANSACTION STATISTICS~n~n"),

    Amounts = [120, 350, 80, 2400, 15, 200, 75, 9800, 45, 180,
               300, 55, 420, 12000, 90, 250, 180, 3500, 60, 110],

    io:format("  Dataset: ~w values~n", [length(Amounts)]),
    io:format("  Mean:    $~.2f~n",    [mean(Amounts)]),
    io:format("  Median:  $~.2f~n",    [median(Amounts)]),
    io:format("  Std Dev: $~.2f~n",    [std_dev(Amounts)]),
    io:format("  Min:     $~w~n",      [lists:min(Amounts)]),
    io:format("  Max:     $~w~n",      [lists:max(Amounts)]),
    io:format("  Total:   $~w~n",      [lists:sum(Amounts)]),

    Totals = running_totals(Amounts, 0),
    io:format("  Running total after all: $~w~n", [lists:last(Totals)]),

    %% ---- Section 5: List Processing ----
    io:format("~n[ 5 ] ERLANG LIST PROCESSING~n~n"),

    Nums = lists:seq(1, 20),
    Squares = lists:map(fun(X) -> X * X end, Nums),
    Evens = lists:filter(fun(X) -> X rem 2 =:= 0 end, Nums),
    Sum100 = lists:foldl(fun(X, Acc) -> X + Acc end, 0, lists:seq(1, 100)),

    io:format("  [1..20] squares: ~w~n", [Squares]),
    io:format("  [1..20] evens:   ~w~n", [Evens]),
    io:format("  Sum 1..100 = ~w (Gauss: n(n+1)/2 = ~w)~n", [Sum100, 100*101 div 2]),

    %% ---- Section 6: Primes ----
    io:format("~n[ 6 ] PRIME SIEVE~n~n"),
    Primes = sieve(100),
    io:format("  Primes up to 100 (~w total):~n  ~w~n", [length(Primes), Primes]),

    %% Twin primes
    Twins = [{P, P+2} || P <- Primes, lists:member(P+2, Primes)],
    io:format("  Twin prime pairs: ~w~n", [Twins]),

    %% ---- Section 7: Pattern matching showcase ----
    io:format("~n[ 7 ] PATTERN MATCHING SHOWCASE~n~n"),
    Balances = [0, -50, 50, 500, 5000, 50000],
    lists:foreach(fun(B) ->
        io:format("    balance ~6w → ~w~n", [B, classify_balance(B)])
    end, Balances),

    io:format("~n============================================================~n"),
    io:format("  Erlang Actor Bank Demo Complete!~n"),
    io:format("  Pattern matching | Higher-order fns | Maps | Recursion~n"),
    io:format("============================================================~n").
