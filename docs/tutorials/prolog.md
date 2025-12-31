# Prolog Programming with Time Warp IDE

Prolog is a unique logic programming language based on first-order logic. Time Warp IDE provides experimental Prolog support for exploring declarative, rule-based programming.

## Quick Start

A simple Prolog program:

```prolog
parent(tom, bob).
parent(bob, ann).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

?- ancestor(tom, ann).
```

This queries whether tom is an ancestor of ann.

## Basic Concepts

### Facts

Facts are statements about the world:

```prolog
parent(tom, bob).
parent(bob, ann).
parent(bob, pat).

student(alice).
student(bob).
student(charlie).
```

### Rules

Rules define relationships using `:- ` (if):

```prolog
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
```

### Queries

Ask questions using `?-`:

```prolog
?- parent(tom, bob).        % Is tom a parent of bob?
?- parent(tom, X).          % Who are tom's children?
?- parent(X, bob).          % Who is bob's parent?
?- grandparent(tom, X).     % Who is tom's grandchild?
```

## Unification and Variables

Variables start with uppercase letters:

```prolog
% Find all children of tom
?- parent(tom, X).
X = bob;
X = alice;
...

% Find all parent-child pairs
?- parent(X, Y).
X = tom, Y = bob;
X = tom, Y = alice;
...

% Multiple conditions (conjunction - AND)
?- parent(tom, X), student(X).
```

## Common Predicates

### Comparison

```prolog
% Unification
X = Y           % Are X and Y the same?
X \= Y          % Are X and Y different?

% Arithmetic comparison
X =:= Y         % Are values equal?
X =\= Y         % Are values different?
X < Y           % Is X less than Y?
X > Y           % Is X greater than Y?
X =< Y          % Is X less than or equal to Y?
X >= Y          % Is X greater than or equal to Y?
```

### List Operations

```prolog
% List matching
[H|T] = [1, 2, 3].     % H=1, T=[2,3]

% Membership
member(X, [1, 2, 3]).  % Is X a member?

% Append
append([1, 2], [3, 4], L).  % L = [1, 2, 3, 4]

% Length
length([1, 2, 3], N).   % N = 3
```

## Complete Example Programs

### Family Relationships

```prolog
% Facts about parents
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

% Rules about relationships
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

greatgrandparent(X, Z) :- parent(X, Y), grandparent(Y, Z).

sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% Queries:
% ?- parent(tom, X).           % Who are tom's children?
% ?- grandparent(tom, X).      % Who are tom's grandchildren?
% ?- ancestor(tom, X).         % All descendants of tom?
% ?- sibling(bob, X).          % Who are bob's siblings?
```

### Math and Logic

```prolog
% Factorial
factorial(0, 1).
factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.

% Fibonacci
fib(0, 0).
fib(1, 1).
fib(N, F) :- N > 1, N1 is N - 1, N2 is N - 2, fib(N1, F1), fib(N2, F2), F is F1 + F2.

% Sum of list
sum_list([], 0).
sum_list([H|T], Sum) :- sum_list(T, TSum), Sum is H + TSum.

% Maximum of list
max_list([X], X).
max_list([H|T], Max) :- max_list(T, MaxT), (H > MaxT -> Max = H ; Max = MaxT).

% Queries:
% ?- factorial(5, F).          % Calculate 5!
% ?- fib(10, F).               % Calculate Fibonacci(10)
% ?- sum_list([1,2,3,4,5], S). % Sum a list
% ?- max_list([3,1,4,1,5], M). % Find maximum
```

### List Processing

```prolog
% Reverse a list
reverse([], []).
reverse([H|T], R) :- reverse(T, RT), append(RT, [H], R).

% Sort (simple version)
sorted([]).
sorted([_]).
sorted([X, Y|T]) :- X =< Y, sorted([Y|T]).

% Find minimum
min([X], X).
min([H|T], Min) :- min(T, MinT), (H < MinT -> Min = H ; Min = MinT).

% Count occurrences
count([], _, 0).
count([H|T], H, N) :- !, count(T, H, N1), N is N1 + 1.
count([_|T], X, N) :- count(T, X, N).

% Queries:
% ?- reverse([1,2,3], R).                 % Reverse a list
% ?- sorted([1,2,3,4]).                   % Check if sorted
% ?- min([5,2,8,1,9], M).                 % Find minimum
% ?- count([a,b,a,c,a], a, N).            % Count 'a' occurrences
```

### Knowledge Base: Animals

```prolog
% Facts about animals
animal(dog).
animal(cat).
animal(bird).
animal(fish).

% Properties
mammal(dog).
mammal(cat).
mammal(whale).

carnivore(dog).
carnivore(cat).
carnivore(lion).

can_fly(bird).
can_fly(eagle).
can_fly(bat).

% Rules
is_pet(dog).
is_pet(cat).
is_pet(bird).

endangered(panda).
endangered(tiger).
endangered(whale).

% Complex rules
safe_pet(X) :- is_pet(X), \+ dangerous(X).

dangerous(lion).
dangerous(snake).
dangerous(tiger).

% Queries:
% ?- mammal(X).               % What mammals do we know?
% ?- can_fly(X).              % What can fly?
% ?- is_pet(X).               % What are pets?
% ?- carnivore(X), is_pet(X). % Carnivorous pets?
```

### Simple Expert System

```prolog
% Medical diagnosis example
symptom(person1, fever).
symptom(person1, cough).
symptom(person1, headache).

symptom(person2, fever).
symptom(person2, aches).

% Diagnoses
diagnosis(cold) :- symptom(X, fever), symptom(X, cough).
diagnosis(flu) :- symptom(X, fever), symptom(X, aches).

% Rules for recommendation
recommend(rest) :- diagnosis(cold).
recommend(rest) :- diagnosis(flu).
recommend(doctor) :- diagnosis(meningitis).

% Queries:
% ?- diagnosis(X).    % What diagnoses match?
% ?- recommend(X).    % What do we recommend?
```

## Important Predicates

### Arithmetic

```prolog
is/2          % Evaluate arithmetic: X is 2 + 3
=:=/2         % Arithmetic equality: 2 + 3 =:= 5
=\=/2         % Arithmetic inequality
</2, >/2      % Comparison
=</2, >=/2    % Comparison with equals
```

### Control

```prolog
true          % Always succeeds
fail          % Always fails
!/0           % Cut (prevent backtracking)
->/2 ; /2     % If-then-else: (condition -> true_part ; false_part)
```

### List Operations

```prolog
append/3      % Append lists
member/2      % Check membership
length/2      % List length
reverse/2     % Reverse a list
sort/2        % Sort a list
nth0/3        % Get nth element (0-indexed)
nth1/3        % Get nth element (1-indexed)
```

## Tips and Best Practices

1. **Start with facts**: Build a solid knowledge base
2. **Use clear names**: `parent(X, Y)` is better than `p(X, Y)`
3. **Test queries**: Verify your rules work
4. **Use comments**: Explain complex rules
5. **Think recursively**: Prolog naturally handles recursive definitions

## Common Patterns

### Pattern Matching

```prolog
process([]).                          % Base case
process([H|T]) :- process_one(H), process(T).  % Recursive case
```

### Recursive List Processing

```prolog
% Sum
sum([], 0).
sum([H|T], S) :- sum(T, ST), S is H + ST.

% Map (apply rule to each)
map([], []).
map([H|T], [R|RT]) :- process(H, R), map(T, RT).
```

### Backtracking

```prolog
choice(a).
choice(b).
choice(c).

% Prolog automatically backtracks through all choices
?- choice(X).
```

## Debugging Tips

```prolog
% Use trace to see execution
% ?- trace.

% Use write to debug
debug_rule(X) :- 
    write('Checking: '), write(X), nl,
    process(X),
    write('Success: '), write(X), nl.

% Write output
nl                % Newline
write(Term)       % Write term
writeln(Term)     % Write term with newline
tab(N)            % Write N spaces
```

## Learning Path

1. **Start**: Create simple facts and queries
2. **Basic Rules**: Write predicates with one condition
3. **Multiple Conditions**: Use conjunctions (,)
4. **Recursion**: Define recursive rules
5. **Advanced**: List processing, backtracking, cuts

## Limitations

- **No mutable state**: Variables can't change
- **No loops**: Use recursion instead
- **Performance**: Can be slow with deep recursion
- **Limited I/O**: Simple read/write operations

## Common Mistakes to Avoid

```prolog
% ❌ Wrong: Variables must be uppercase
parent(tom, bob).
?- parent(tom, X).  % X works
?- parent(tom, x).  % x is a constant, not a variable!

% ✅ Right: Use uppercase for variables
?- parent(tom, Child).

% ❌ Wrong: Forgetting the rule structure
grandparent(X, Z) parent(X, Y), parent(Y, Z).  % Error!

% ✅ Right: Use :- for rules
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

% ❌ Wrong: Not providing all needed facts
?- parent(unknown, bob).  % Fails - unknown person unknown

% ✅ Right: Add the fact first
parent(unknown, bob).
?- parent(unknown, bob).  % Now succeeds
```

## Running Prolog Programs in Time Warp IDE

1. Create a `.pl` or `.pro` file with your program
2. Select "Prolog" from the language dropdown
3. Paste your code or load the file
4. Type queries in the format `?- query.`
5. Press Enter to execute

## Next Steps

- Learn [Logic programming concepts](../reference/builtins.md)
- Explore [Expert systems and knowledge bases]
- Try [Python for general programming](python.md)
- Learn [BASIC for procedural programming](basic.md)

Happy Prolog programming!
