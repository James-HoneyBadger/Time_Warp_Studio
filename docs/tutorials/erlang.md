# Erlang Tutorial

## Introduction

Erlang is a concurrent, functional programming language designed for building massively scalable, fault-tolerant systems. Developed at Ericsson in the 1980s, it powers telecommunications infrastructure, messaging platforms, and databases worldwide (WhatsApp, RabbitMQ, CouchDB).

**File extension:** `.erl`

---

## Hello World

```erlang
-module(hello).
-export([main/0]).

main() ->
    io:format("Hello, World!~n").
```

---

## Module Structure

Every Erlang file is a module:

```erlang
-module(mymodule).           % module name (must match filename)
-export([main/0, add/2]).    % exported function/arity pairs

main() ->
    io:format("Running~n").

add(X, Y) ->
    X + Y.
```

---

## Basic Data Types

```erlang
% Atoms: lowercase identifiers or quoted
ok
error
'hello world'

% Integers and floats
42
3.14

% Strings (lists of integers in Erlang)
"Hello"

% Booleans (atoms)
true
false

% Tuples: fixed-size containers
{point, 3, 4}
{ok, "success"}
{error, "not found"}

% Lists
[1, 2, 3, 4, 5]
[Head | Tail]    % Head = first element, Tail = rest
```

---

## Variables and Pattern Matching

Variables in Erlang are **single-assignment** and start with uppercase:

```erlang
X = 42,           % bind X to 42
X = 42,           % OK — same value
% X = 99,         % ERROR — X is already bound

{ok, Value} = {ok, "hello"},   % pattern match
io:format("Value: ~s~n", [Value]).

% Tuple destructuring
Point = {3, 4},
{X2, Y2} = Point,
io:format("x=~w y=~w~n", [X2, Y2]).
```

---

## Functions and Clauses

Functions can have multiple clauses with pattern matching:

```erlang
% Pattern matching clauses
factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

% Guards (when clause)
classify(N) when N < 0  -> negative;
classify(0)             -> zero;
classify(N) when N > 0  -> positive.

% Main entry
main() ->
    io:format("5! = ~w~n", [factorial(5)]),
    io:format("~w~n", [classify(-3)]),
    io:format("~w~n", [classify(0)]),
    io:format("~w~n", [classify(7)]).
```

---

## Case and If Expressions

```erlang
% case
describe(X) ->
    case X of
        0     -> "zero";
        1     -> "one";
        N when N < 0 -> "negative";
        _     -> "other"
    end.

% if (all branches must have guard-like conditions)
sign(X) ->
    if
        X < 0  -> negative;
        X == 0 -> zero;
        X > 0  -> positive
    end.
```

---

## Lists

```erlang
% List construction and head/tail
L = [1, 2, 3, 4, 5],
[H | T] = L,
io:format("Head: ~w, Tail: ~w~n", [H, T]).

% Common list operations (lists module)
Len    = length(L),
Rev    = lists:reverse(L),
Mapped = lists:map(fun(X) -> X * 2 end, L),
Filt   = lists:filter(fun(X) -> X rem 2 == 0 end, L),
Sum    = lists:sum(L),
Max    = lists:max(L),
Sorted = lists:sort(L).

% List comprehension
Squares = [X * X || X <- lists:seq(1, 10)],
Evens   = [X || X <- lists:seq(1, 20), X rem 2 == 0].
```

---

## Recursion

Erlang uses recursion instead of loops:

```erlang
% Sum a list
sum([])     -> 0;
sum([H|T])  -> H + sum(T).

% Reverse a list
reverse(L) -> reverse(L, []).
reverse([], Acc)    -> Acc;
reverse([H|T], Acc) -> reverse(T, [H|Acc]).

% Map over a list
my_map(_, [])     -> [];
my_map(F, [H|T])  -> [F(H) | my_map(F, T)].
```

---

## Tuples and Records

```erlang
% Tuples
Point = {3, 4},
{X, Y} = Point,
Distance = math:sqrt(X*X + Y*Y).

% Tagged tuples (convention)
Result = {ok, 42},
case Result of
    {ok, Val}  -> io:format("Success: ~w~n", [Val]);
    {error, E} -> io:format("Error: ~w~n", [E])
end.
```

---

## Higher-Order Functions and fun

```erlang
% Anonymous function (fun)
Double = fun(X) -> X * 2 end,
io:format("~w~n", [Double(5)]).    % 10

% Pass fun to lists:map
Tripled = lists:map(fun(X) -> X * 3 end, [1, 2, 3, 4, 5]).

% Function reference: fun module:name/arity
lists:foreach(fun io:format/1, ["one\n", "two\n", "three\n"]).
```

---

## String Operations

```erlang
% io:format format specifiers
io:format("~w~n",  [42]),           % ~w = any term
io:format("~p~n",  [{a, b, c}]),    % ~p = pretty print
io:format("~s~n",  ["hello"]),      % ~s = string
io:format("~d~n",  [255]),          % ~d = decimal integer
io:format("~f~n",  [3.14]),         % ~f = float
io:format("~e~n",  [3.14]),         % ~e = scientific notation

% string module
S = "Hello, World!",
Upper = string:to_upper(S),
Lower = string:to_lower(S),
Len   = string:len(S),
Sub   = string:sub_string(S, 1, 5).  % Erlang strings are 1-indexed
```

---

## Processes and Concurrency

Erlang's killer feature is lightweight processes:

```erlang
% Spawn a process
Pid = spawn(fun() ->
    io:format("Hello from process ~w~n", [self()])
end),

% Send a message
Pid ! {hello, "world"},

% Receive a message
receive
    {hello, Msg} ->
        io:format("Got: ~s~n", [Msg]);
    Other ->
        io:format("Unknown: ~w~n", [Other])
after 1000 ->
    io:format("Timeout~n")
end.
```

---

## Error Handling

```erlang
% try/catch
safe_divide(X, 0) ->
    {error, division_by_zero};
safe_divide(X, Y) ->
    {ok, X / Y}.

% try expression
Result = try
    10 / 0
catch
    error:badarith -> infinity
end.

% Using ok/error tuples (preferred in Erlang)
case safe_divide(10, 2) of
    {ok, Val}   -> io:format("Result: ~w~n", [Val]);
    {error, Msg} -> io:format("Error: ~w~n", [Msg])
end.
```

---

## Tips

- Erlang variable names start with uppercase; atoms start with lowercase
- `;` separates function clauses; `.` ends a function definition
- `,` separates expressions within a clause
- Pattern matching is the primary way to control program flow
- "Let it crash" philosophy: don't over-protect, use supervisors instead
- The `lists` module has most functional programming primitives
