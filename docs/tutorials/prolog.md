# Prolog Tutorial

**Prolog** (Logic Programming) is a fundamentally different language based on logic and pattern matching. Instead of telling the computer *how* to do something, you tell it *what* is true.

## Getting Started

### Hello, World! 👋

```prolog
?- write('Hello, World!'), nl.
```

**Output:**
```
Hello, World!
true.
```

`?-` means "query" (ask a question)
`write()` prints text
`nl` adds newline
`.` ends the query

## Facts and Queries

### Simple Facts

```prolog
% Facts
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).
```

`%` starts a comment

### Querying Facts

```prolog
?- parent(tom, bob).
```

**Output:**
```
true.
```

The query succeeds because `parent(tom, bob)` is a fact.

```prolog
?- parent(tom, ann).
```

**Output:**
```
false.
```

### Multiple Solutions

```prolog
?- parent(tom, X).
```

**Output:**
```
X = bob ;
X = liz.
```

Prolog finds all values of X where `parent(tom, X)` is true.

## Rules

Rules define relationships:

```prolog
% Facts
parent(tom, bob).
parent(bob, ann).

% Rule: X is a grandparent of Z if X is parent of Y and Y is parent of Z
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

`:-` means "if"
`,` means "and"

### Querying Rules

```prolog
?- grandparent(tom, ann).
```

**Output:**
```
true.
```

Because `parent(tom, bob)` AND `parent(bob, ann)`.

### Ancestor Rule

```prolog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

This recursive rule says:
1. X is an ancestor of Y if X is a parent of Y, OR
2. X is an ancestor of Y if X is a parent of Z and Z is an ancestor of Y

## Arithmetic

### Comparison

```prolog
?- 5 > 3.
true.

?- 10 < 5.
false.

?- X = 5.
X = 5.
```

### Arithmetic Operations

```prolog
?- X is 2 + 3.
X = 5.

?- X is 10 - 4.
X = 6.

?- X is 3 * 4.
X = 12.

?- X is 20 / 4.
X = 5.
```

The `is` operator evaluates arithmetic.

### Using in Facts

```prolog
age(john, 25).
age(alice, 30).
age(bob, 20).

adult(X) :- age(X, Age), Age >= 18.

?- adult(john).
true.
```

## Lists

### Basic List Syntax

```prolog
?- L = [1, 2, 3].
L = [1, 2, 3].

?- L = [1, [2, 3], 4].
L = [1, [2, 3], 4].

?- L = [a | Rest].
L = [a|Rest].  (Rest is unbound)
```

`[H | T]` means head and tail: H is first element, T is rest

### List Operations

```prolog
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

?- append([1,2], [3,4], X).
X = [1,2,3,4].

?- append([a], [b,c], X).
X = [a,b,c].
```

### Member Predicate

```prolog
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

?- member(2, [1,2,3]).
true.

?- member(X, [a,b,c]).
X = a ;
X = b ;
X = c.
```

## Complete Example: Family Tree

```prolog
% Male and female facts
male(tom).
male(bob).
male(jim).
male(pat).

female(liz).
female(ann).
female(sue).

% Parent facts
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).
parent(pat, sue).

% Rules
father(X, Y) :- male(X), parent(X, Y).
mother(X, Y) :- female(X), parent(X, Y).

grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
grandfather(X, Y) :- male(X), grandparent(X, Y).
grandmother(X, Y) :- female(X), grandparent(X, Y).

sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% Queries
?- father(tom, X).
?- grandfather(tom, X).
?- ancestor(tom, X).
?- sibling(pat, X).
```

## Complete Example: List Operations

```prolog
% Check if list is empty
empty([]).

% Get length of list
length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

% Reverse a list
reverse([], []).
reverse([H|T], R) :- reverse(T, RevT), append(RevT, [H], R).

% Find maximum in list
max([X], X).
max([H|T], M) :- max(T, M1), (H > M1 -> M = H ; M = M1).

% Find minimum in list
min([X], X).
min([H|T], M) :- min(T, M1), (H < M1 -> M = H ; M = M1).

% Sum of list
sum([], 0).
sum([H|T], S) :- sum(T, S1), S is H + S1.

% Queries
?- length([1,2,3,4], N).
N = 4.

?- reverse([1,2,3], R).
R = [3,2,1].

?- sum([1,2,3,4,5], S).
S = 15.
```

## Control Flow

### Cut (!)

```prolog
max(X, Y, X) :- X >= Y, !.
max(X, Y, Y).

?- max(5, 3, M).
M = 5.
```

The `!` (cut) prevents backtracking.

### Conditional (->)

```prolog
sign(X, positive) :- X > 0, !.
sign(X, negative) :- X < 0, !.
sign(0, zero).

% Or using conditional:
sign2(X, S) :- (X > 0 -> S = positive ; X < 0 -> S = negative ; S = zero).

?- sign(5, S).
S = positive.
```

## Common Patterns

### NOT Operator

```prolog
not_parent(X, Y) :- \+ parent(X, Y).

?- not_parent(alice, bob).
true.
```

`\+` means "NOT"

### Negation as Failure

```prolog
single(X) :- \+ parent(_, X).

?- single(X).  (finds people with no parents)
```

### Iteration Over List

```prolog
process_list([]).
process_list([H|T]) :-
  write(H), nl,
  process_list(T).

?- process_list([1,2,3]).
1
2
3
true.
```

## Common Commands Reference

### Basic
| Term | Meaning | Example |
|------|---------|---------|
| `:-` | "if" or "rule" | `rule :- fact.` |
| `,` | "and" | `fact1, fact2` |
| `;` | "or" | `option1 ; option2` |
| `.` | ends clause | `fact.` |
| `%` | comment | `% This is a comment` |

### Operators
| Operator | Meaning | Example |
|----------|---------|---------|
| `=` | unify | `X = 5` |
| `is` | evaluate | `X is 2 + 3` |
| `>`, `<` | comparison | `X > 5` |
| `>=`, `=<` | comparison | `X >= 5` |
| `\=` | not unifiable | `X \= 5` |
| `\+` | not provable | `\+ fact` |

### List Operators
| Pattern | Meaning | Example |
|---------|---------|---------|
| `[]` | empty list | `L = []` |
| `[H\|T]` | head/tail | `L = [1\|[2,3]]` |
| `[H\|T]` | match first | `member(X, [1\|Rest])` |

## Tips for Learning Prolog

1. **Think declaratively** - State what is true, not how to compute
2. **Build facts first** - Start with data
3. **Write simple rules** - Build complexity gradually
4. **Test queries** - Ask specific questions
5. **Use recursion** - Process lists recursively
6. **Debug with trace** - Use tracing to understand execution

## Common Mistakes

| Mistake | Problem | Fix |
|---------|---------|-----|
| `parent(tom, bob)` as query | Asks is it true | Use `?-` for query |
| `:-` in queries | Rule in wrong place | Only use in clauses |
| Variable starts lowercase | Not a variable | Variables start UPPERCASE |
| Forgetting `is` | Math not evaluated | `X is 2 + 3` not `X = 2 + 3` |
| Infinite recursion | Stack overflow | Check base case |

## Prolog vs Other Languages

| Feature | Prolog | BASIC/Pascal |
|---------|--------|-------------|
| Paradigm | Logic | Imperative |
| How | Declare relations | Give instructions |
| Queries | Pattern matching | Function calls |
| Lists | Native | Arrays |
| Recursion | Essential | Optional |

## Next Steps

1. ✅ Learn facts and rules
2. ✅ Practice queries
3. ✅ Build family tree program
4. ✅ Process lists recursively
5. 📂 Try examples from `Examples/prolog/`

---

Happy logic programming! 🧠
