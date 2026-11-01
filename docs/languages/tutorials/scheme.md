# Scheme: a program is a list

Scheme is a dialect of Lisp, designed in 1975 by Guy Steele and Gerald Sussman at
MIT to be a small language with a precise definition. A program is made of lists,
and a list is a call: the first item is the procedure and the rest are its
arguments. Recursion and procedures-as-values do the work that other languages give
to loops and classes. Here it runs as a whole program on an explicit-stack machine,
so the depth a recursive procedure can reach is set by available memory.

Press **F9** to run a document. The output blocks below are what the program prints.

## What a program looks like

```scheme
(display "Hello, World!")
(newline)
```

```text
Hello, World!
```

`display` writes a value and `newline` ends the line. The operator comes first, which
is the one thing to learn before anything else: `(+ 1 2)` adds. Nothing is printed
unless a program asks, so a file of definitions produces no output at all.

## Step 1: prefix arithmetic

Add, multiply, divide and subtract with the operator in front.

```scheme
(display (list (+ 1 2 3) (* 6 7) (/ 10 4) (- 10 4 1)))
(newline)
```

```text
(6 42 2.5 5)
```

A procedure takes as many arguments as it is given: `+` with three numbers adds
three, `-` with two subtracts and `-` with three subtracts both. Division is exact
where it can be, so `(/ 10 4)` is `2.5` rather than `2`. There is no precedence to
learn because there is no infix: the parentheses say what is applied to what.

## Step 2: definitions

Give a value a name, then define a procedure.

```scheme
(define pi 3.14159)
(define (square n) (* n n))
(display pi)
(newline)
(display (square 7))
(newline)
(display ((lambda (x) (* x x)) 9))
(newline)
```

```text
3.14159
49
81
```

`(define (square n) …)` is shorthand for `(define square (lambda (n) …))`, and the
long form can be used directly, as the third expression does: a `lambda` is a
procedure, and a procedure in the first position is called. `define` at the top level
of a file adds to the environment, and the REPL bar keeps those bindings between
entries, so a definition typed there survives.

## Step 3: conditionals

Choose a branch with `cond`, and with `if` when there are only two.

```scheme
(define (sign n)
  (cond ((> n 0) "positive")
        ((< n 0) "negative")
        (else "zero")))
(display (sign -3))
(newline)
(display (sign 0))
(newline)
(define (abs-value n) (if (< n 0) (- n) n))
(display (abs-value -7))
(newline)
```

```text
negative
zero
7
```

`cond` takes a list of clauses, each a test and the value to answer when the test is
true; `else` catches the rest. `if` takes exactly three parts: a test, the value when
it is true and the value when it is false. Both answer a value rather than performing
an action, so `abs-value` returns its answer as an expression and prints nothing until
the caller displays it.

## Step 4: lists

Build a list and ask it questions.

```scheme
(define xs (list 1 2 3 4 5))
(display (car xs))
(newline)
(display (cdr xs))
(newline)
(display (map (lambda (x) (* x x)) xs))
(newline)
(display (filter odd? xs))
(newline)
(display (apply + xs))
(newline)
```

```text
1
(2 3 4 5)
(1 4 9 16 25)
(1 3 5)
15
```

`car` is the first element and `cdr` the rest, so a list is walked by taking one and
recurring on the other. `map` applies a procedure to every element, `filter` keeps
the ones whose test is true, and `apply` calls a procedure with the list as its
arguments. A list printed by `display` shows its parentheses because that is the
value: lists are both data and the shape of a program.

## Step 5: recursion and a loop

Multiply a factorial out by recursion, then count with a named `let`.

```scheme
(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
(display (fact 5))
(newline)
(let loop ((i 1))
  (when (<= i 4)
    (display i)
    (newline)
    (loop (+ i 1))))
```

```text
120
1
2
3
4
```

A recursive procedure is the ordinary way to repeat something: `fact` calls itself
with a smaller number and multiplies on the way back, and the `if` is both the
answer and the stopping rule. `let loop` names a block and calls it, which is how a
loop is written without mutation; `when` runs its body only while the test holds.

## Step 6: the turtle

Draw a hexagon by walking and turning.

```scheme
(define (polygon sides length)
  (let loop ((n 0))
    (when (< n sides)
      (forward length)
      (right (/ 360 sides))
      (loop (+ n 1)))))
(polygon 6 80)
```

```text
-- 6 line(s), 0 shape(s)
```

The turtle words are `forward` (or `fd`), `backward` (or `bk`), `right`, `left`,
`goto`, `penup`, `pendown`, `home`, `circle` and `color`, which takes red, green and
blue as numbers from 0 to 255. The angle is computed rather than written, so the
same procedure draws any regular polygon: six sides turn by 360 divided by six.

## Where it catches people out

**The operator is not in the middle.** `(1 + 2)` is a call to the number 1, which
reports `attempt to call non-procedure: 1` and stops the run; the arithmetic that
looks natural in every other language is not Scheme.

```scheme
(display (1 + 2))
```

`(display (+ 1 2))` is the program that prints 3.

**`display` does not end the line.** Two displays run together, which is useful for
building a line and wrong when a line per value was wanted.

```scheme
(display 1)
(display 2)
```

```text
12
```

Put `(newline)` after the last value of each line. This is the most common reason a
first Scheme program prints one long ribbon.

**A list has to be quoted to be data.** `(a b)` is a call to `a`; `'(a b)` is the
list. `(list 'a 'b)` builds the same thing from values, and `quote` before a list is
what makes `(display '(a b))` print `(a b)`.

## Forms used here

| Form | What it does |
| --- | --- |
| `(display x)`, `(newline)` | Write a value, end a line |
| `(define name value)` | Bind a name |
| `(define (f a) …)` | Define a procedure |
| `(lambda (a) …)` | An anonymous procedure |
| `(if test then else)` | Two-way branch |
| `(cond (test value) … (else …))` | Multi-way branch |
| `(let ((name value)) …)`, `let loop` | Local bindings, and a loop |
| `(car xs)`, `(cdr xs)` | First element, and the rest |
| `(map f xs)`, `(filter p xs)`, `(apply f xs)` | Apply a procedure across a list |
| `'x`, `(quote x)` | The expression as data |
| `(forward n)`, `(right d)` | Drive the turtle |

## Next

The [language guide](../reference.md#scheme) covers `set!`, closures, `when`
and `unless`, and the rest of the turtle.
[`Examples/scheme/recursion.scm`](../../../Examples/scheme/recursion.scm) and
[`Examples/scheme/higher_order.scm`](../../../Examples/scheme/higher_order.scm) take
these ideas further.
