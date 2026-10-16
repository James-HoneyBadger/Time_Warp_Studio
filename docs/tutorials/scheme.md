# Scheme Tutorial

Scheme is a small, elegant dialect of Lisp. Programs are built from
**S-expressions**: parenthesised lists in which the operator always comes
first. It is an excellent language for learning recursion, higher-order
procedures, and functional thinking.

Time Warp Studio runs Scheme as a whole program.

---

## 1. Displaying output

```scheme
(display "Hello, World!")
(newline)
```

`display` writes a value; `newline` ends the line. `write` is like `display`
but quotes strings and `#\` characters.

## 2. Prefix arithmetic

The operator comes first and accepts any number of arguments:

```scheme
(display (+ 1 2 3))        →  6
(display (* 6 7))          →  42
(display (- 10 4 1))       →  5
(display (/ 10 4))         →  2.5
```

## 3. Defining values and procedures

```scheme
(define pi 3.14159)
(define (square n) (* n n))

(display (square 7))       →  49
(newline)
```

Anonymous procedures use `lambda`:

```scheme
(display ((lambda (x) (* x x)) 9))     →  81
```

## 4. Conditionals

```scheme
(define (sign n)
  (cond ((> n 0) "positive")
        ((< n 0) "negative")
        (else "zero")))

(display (sign -3))
(newline)
```

`if` takes exactly two branches:

```scheme
(define (abs-value n)
  (if (< n 0) (- n) n))
```

`and`, `or`, `not`, `when` and `unless` are also available.

## 5. Recursion

Repetition in Scheme is normally expressed as recursion:

```scheme
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(display (fact 5))         →  120
(newline)
```

## 6. Lists

```scheme
(define xs (list 1 2 3 4 5))

(display (car xs))              →  1
(display (cdr xs))              →  (2 3 4 5)
(display (length xs))           →  5
(display (reverse xs))          →  (5 4 3 2 1)
(display (append xs (list 6)))  →  (1 2 3 4 5 6)
```

A quoted list is data, not code:

```scheme
(display '(a b c))              →  (a b c)
```

## 7. Higher-order procedures

```scheme
(display (map (lambda (x) (* x x)) (list 1 2 3 4)))   →  (1 4 9 16)
(display (filter odd? (list 1 2 3 4 5)))              →  (1 3 5)
(display (apply + (list 10 20 30)))                   →  60
```

## 8. Local bindings

```scheme
(display (let ((x 3) (y 4)) (+ x y)))      →  7
```

A *named* `let` is the idiomatic way to write a loop:

```scheme
(let loop ((i 0))
  (when (< i 5)
    (display i)
    (loop (+ i 1))))
```

## 9. Closures

Procedures remember the environment in which they were created:

```scheme
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add10 (make-adder 10))
(display (add10 5))        →  15
```

## 10. Turtle graphics

| Procedure | Effect |
| --- | --- |
| `(forward n)` / `(fd n)` | Move forward |
| `(backward n)` / `(back n)` | Move backward |
| `(right d)` / `(left d)` | Turn |
| `(goto x y)` | Jump to a point |
| `(penup)` / `(pendown)` | Lift / lower the pen |
| `(home)` | Return to the origin |
| `(circle r)` | Draw a circle |
| `(color r g b)` | Set the pen colour |

```scheme
(define (polygon sides length)
  (define (loop n)
    (when (> n 0)
      (forward length)
      (right (/ 360 sides))
      (loop (- n 1))))
  (loop sides))

(polygon 6 80)
```

## 11. Comments

A semicolon starts a comment to the end of the line.

```scheme
; draw a triangle
(polygon 3 60)
```

---

## Quick reference

| Form | Purpose |
| --- | --- |
| `(define …)` | Bind a value or procedure |
| `(lambda (args) body)` | Anonymous procedure |
| `(if test then else)` | Two-way branch |
| `(cond …)` | Multi-way branch |
| `(let …)` / `(let loop …)` | Local bindings / loops |
| `(set! var value)` | Mutate a binding |
| `(quote x)` / `'x` | Literal data |
| `(display x)` / `(newline)` | Output |
| `(map f lst)`, `(filter p lst)`, `(foldl f init lst)` | List processing |
