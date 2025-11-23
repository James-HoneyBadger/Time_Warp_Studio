# Prolog Programming Examples

Educational Prolog programs demonstrating logic programming and various techniques.

## Files Overview

| File | Topic | Techniques Demonstrated |
|------|-------|------------------------|
| `01_facts_rules.pl` | Facts & Rules | Knowledge base, queries, logical inference |
| `02_family_tree.pl` | Relationships | Complex rules, recursion, family relations |
| `03_lists.pl` | List Operations | Head/tail, recursion, list predicates |
| `04_recursion.pl` | Recursion | Base cases, recursive definitions, backtracking |
| `05_arithmetic.pl` | Arithmetic | Math operations, `is`, comparisons |
| `06_pattern_matching.pl` | Patterns | Structures, unification, term manipulation |
| `07_logic_puzzles.pl` | Logic Puzzles | Constraint solving, generate & test |
| `08_sorting.pl` | Sorting Algorithms | Multiple sort algorithms, performance |

## Running Prolog Programs

### Using SWI-Prolog
```bash
# Start SWI-Prolog with a file
swipl -s 01_facts_rules.pl

# Inside Prolog, make queries:
?- likes(alice, programming).
?- happy(X).
?- halt.  % to exit
```

### Using Time Warp IDE
1. Open the `.pl` file in Time Warp IDE
2. Click "Run" or press F5
3. Enter queries in the console
4. Use `;` to see more solutions

## Concepts Covered

### Facts and Rules
- Facts: `likes(alice, programming).`
- Rules: `happy(X) :- likes(X, Y), fun(Y).`
- Queries: `?- likes(alice, X).`

### Logic Programming
- Declarative (what, not how)
- Unification and pattern matching
- Backtracking
- Multiple solutions

### Lists
- Notation: `[1, 2, 3]`
- Head and tail: `[H|T]`
- Empty list: `[]`
- List operations (append, reverse, member)

### Recursion
- Base case (stopping condition)
- Recursive case (self-reference)
- Examples: factorial, fibonacci, tree traversal

### Arithmetic
- `is` operator: `X is 2 + 3`
- Comparison: `<`, `>`, `=<`, `>=`, `=:=`
- Operators: `+`, `-`, `*`, `/`, `//`, `mod`

### Pattern Matching
- Structures: `person(name(First, Last), Age)`
- Unification: matching terms
- Anonymous variable: `_`
- Don't care: `_` in patterns

## Learning Path

**Beginners**: Start with files 01-03
- Understand facts and rules
- Learn query syntax
- Practice list operations
- Master recursion basics

**Intermediate**: Continue with files 04-06
- Recursive algorithms
- Arithmetic operations
- Pattern matching with structures

**Advanced**: Study files 07-08
- Constraint solving
- Logic puzzles
- Algorithm implementation
- Performance considerations

## Common Patterns

### Recursive List Processing
```prolog
% Base case
process([], Result).

% Recursive case
process([H|T], Result) :-
    process(T, RestResult),
    % do something with H and RestResult
```

### Fact + Rule Pattern
```prolog
% Direct facts
parent(john, mary).
parent(john, tom).

% Derived rules
grandparent(GP, GC) :-
    parent(GP, P),
    parent(P, GC).
```

### Generate and Test
```prolog
solve(Solution) :-
    generate(Solution),   % Generate candidate
    test(Solution).       % Test if valid
```

## Prolog Syntax

### Facts
```prolog
likes(alice, programming).
age(bob, 25).
```

### Rules
```prolog
happy(Person) :- likes(Person, Activity).
```

### Queries
```prolog
?- likes(alice, X).      % What does Alice like?
?- likes(X, programming). % Who likes programming?
```

### Operators
- `:-` means "if" (implication)
- `,` means "and" (conjunction)
- `;` means "or" (disjunction)
- `\+` means "not" (negation)
- `\=` means "not equal"

## Tips for Success

1. **Think declaratively**: Describe relationships, not procedures
2. **Start with base cases**: Always define stopping conditions
3. **Use meaningful names**: `parent(X,Y)` not `p(X,Y)`
4. **Test incrementally**: Add facts/rules one at a time
5. **Use trace mode**: `trace.` to debug, `notrace.` to stop
6. **Embrace backtracking**: Prolog tries all possibilities

## Query Examples

### Simple Queries
```prolog
?- likes(alice, programming).    % Yes/No question
?- likes(alice, X).               % What does Alice like?
?- likes(X, programming).         % Who likes programming?
```

### Complex Queries
```prolog
?- parent(X, mary), parent(X, tom).  % Common parent
?- ancestor(john, X).                 % All descendants
?- findall(X, likes(X, _), People).  % Collect all people
```

### Arithmetic Queries
```prolog
?- X is 2 + 3.                   % Calculate
?- factorial(5, F).              % Compute factorial
?- between(1, 10, X).            % Generate numbers
```

## Common Predicates

| Predicate | Purpose | Example |
|-----------|---------|---------|
| `member(X, List)` | Check membership | `member(3, [1,2,3])` |
| `append(L1, L2, L3)` | Concatenate | `append([1,2], [3,4], X)` |
| `length(List, N)` | List length | `length([1,2,3], N)` |
| `is` | Arithmetic | `X is 5 + 3` |
| `findall(X, Goal, List)` | Collect solutions | `findall(X, parent(X,_), Parents)` |

## Resources

- [Prolog Programming Guide](../../docs/user/01-programming-guide.md)
- [Quick Reference](../../docs/user/02-quick-reference.md)
- [Student Workbook](../../docs/student/00-workbook.md)

## Practice Exercises

Try extending the examples:
- Add more family relationships (cousin, nephew)
- Implement graph algorithms (shortest path)
- Create a simple expert system
- Build a constraint solver
- Design a puzzle solver
