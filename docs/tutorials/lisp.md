# LISP/Scheme Tutorial

## Introduction

LISP (LISt Processing, 1958) is one of the oldest high-level programming languages still in widespread use. Time Warp Studio implements a Scheme-compatible subset — a clean, minimalist dialect of LISP that is widely taught in computer science courses.

**Key characteristics:**
- Everything is an expression — returns a value
- Code is data: programs are lists (`(operator arg1 arg2)`)
- Powerful recursion and higher-order functions
- Dynamic typing

## Hello World

```scheme
(display "Hello, World!")
(newline)
```

## Variables

```scheme
(define x 42)
(define name "Alice")
(define pi 3.14159)

(display x)
(display name)
```

`define` binds a name to a value in the current environment.

## Arithmetic

```scheme
(display (+ 1 2 3))      ; 6
(display (* 4 5))         ; 20
(display (- 10 3))        ; 7
(display (/ 10 4))        ; 2.5
(display (expt 2 10))     ; 1024
(display (sqrt 16))       ; 4.0
(display (abs -7))        ; 7
(display (modulo 17 5))   ; 2
(display (max 3 1 4 1 5)) ; 5
(display (min 3 1 4 1 5)) ; 1
```

Note the prefix notation: operator comes first, then all arguments.

## Conditionals

```scheme
(define x 42)

; if expression
(if (> x 10)
    (display "Large")
    (display "Small"))

; cond (multi-branch)
(cond
  ((< x 0)  (display "negative"))
  ((= x 0)  (display "zero"))
  (else     (display "positive")))
```

## Functions (lambda)

```scheme
; Anonymous function
(define square (lambda (n) (* n n)))
(display (square 7))    ; 49

; Shorthand definition syntax
(define (cube n) (* n n n))
(display (cube 3))      ; 27

; Multiple arguments
(define (hypotenuse a b)
  (sqrt (+ (* a a) (* b b))))
(display (hypotenuse 3 4))  ; 5.0
```

## Recursion

```scheme
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(display (factorial 6))   ; 720
(display (factorial 10))  ; 3628800

; Fibonacci
(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))))

(display (fib 10))   ; 55
```

## Lists

Lists are the fundamental data structure in LISP.

```scheme
; Build a list
(define lst '(1 2 3 4 5))

; car = first element, cdr = rest of list
(display (car lst))      ; 1
(display (cdr lst))      ; (2 3 4 5)
(display (cadr lst))     ; 2  (car of cdr)

; cons: prepend an element
(display (cons 0 lst))   ; (0 1 2 3 4 5)

; length
(display (length lst))   ; 5

; null? checks for empty list
(display (null? '()))    ; #t
(display (null? lst))    ; #f

; append two lists
(display (append '(1 2) '(3 4)))  ; (1 2 3 4)

; reverse
(display (reverse lst))  ; (5 4 3 2 1)
```

## Higher-Order Functions

```scheme
; map: apply a function to every element
(display (map (lambda (x) (* x x)) '(1 2 3 4 5)))
; (1 4 9 16 25)

; filter: keep elements that satisfy a predicate
(display (filter even? '(1 2 3 4 5 6)))
; (2 4 6)

; fold (reduce): accumulate a result
(define (my-sum lst) (apply + lst))
(display (my-sum '(1 2 3 4 5)))   ; 15
```

## Let Bindings

```scheme
(let ((x 10)
      (y 20))
  (display (+ x y)))   ; 30

; let* allows later bindings to reference earlier ones
(let* ((x 5)
       (y (* x 2)))
  (display y))         ; 10
```

## Boolean Values

```scheme
#t    ; true
#f    ; false

(and #t #f)   ; #f
(or  #f #t)   ; #t
(not #f)      ; #t
```

## Predicates

```scheme
(number? 42)      ; #t
(string? "hi")    ; #t
(list? '(1 2))    ; #t
(null? '())       ; #t
(equal? '(1 2) '(1 2))  ; #t
(zero? 0)         ; #t
(positive? 5)     ; #t
(even? 4)         ; #t
(odd? 3)          ; #t
```

## Quick Reference

| Expression | Meaning |
|------------|---------|
| `(define x val)` | Bind name |
| `(lambda (args) body)` | Anonymous function |
| `(define (f args) body)` | Named function |
| `(if test then else)` | Conditional |
| `(cond (test expr) ...)` | Multi-branch |
| `(let ((x v) ...) body)` | Local binding |
| `(car lst)` | First element |
| `(cdr lst)` | Rest of list |
| `(cons x lst)` | Prepend |
| `(list a b c)` | Build list |
| `(map f lst)` | Transform list |
| `(filter pred lst)` | Filter list |
| `(apply f lst)` | Apply to list |
