# Scheme Programming Tutorial

Scheme is a minimalist dialect of Lisp created by Guy Steele and Gerald Sussman in 1975. Its elegance lies in having very few primitives that combine powerfully.

## Hello World

```scheme
(display "Hello from Scheme!")
(newline)
(display "Welcome to Time Warp Studio")
(newline)
```

## S-Expressions

Everything in Scheme is an **S-expression**: either an atom or a list.

```scheme
; Atoms: numbers, strings, booleans, symbols
42
"hello"
#t        ; true
#f        ; false
'pi       ; symbol

; Lists: (operator arg1 arg2 ...)
(+ 1 2)      ; 3
(* 4 5)      ; 20
(- 10 3)     ; 7
(/ 15 5)     ; 3
(expt 2 10)  ; 1024
```

## Define

```scheme
; Define a variable
(define x 42)
(define name "Alice")
(define pi 3.14159)

(display x) (newline)

; Define a function
(define (square n) (* n n))
(define (cube n) (* n n n))

(display (square 7)) (newline)
(display (cube 4))   (newline)

; Recursive factorial
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(display (factorial 10)) (newline)
```

## Conditionals

```scheme
; if
(if (> 5 3)
    (display "5 is greater")
    (display "5 is not greater"))
(newline)

; cond (multi-branch)
(define (grade score)
  (cond ((>= score 90) "A")
        ((>= score 80) "B")
        ((>= score 70) "C")
        ((>= score 60) "D")
        (else          "F")))

(display (grade 87)) (newline)

; and / or / not
(display (and #t #t #f)) (newline)   ; #f
(display (or  #f #f #t)) (newline)   ; #t
(display (not #f))       (newline)   ; #t
```

## Lists

```scheme
; List construction
(define nums  (list 1 2 3 4 5))
(define mixed '(1 "two" #t 4.0))

; car (head) and cdr (tail)
(display (car nums))  (newline)   ; 1
(display (cdr nums))  (newline)   ; (2 3 4 5)
(display (cadr nums)) (newline)   ; 2  (car of cdr)

; cons (prepend)
(define nums2 (cons 0 nums))
(display nums2) (newline)          ; (0 1 2 3 4 5)

; length, append, reverse
(display (length nums))        (newline)
(display (append nums '(6 7))) (newline)
(display (reverse nums))       (newline)

; Built-in predicates
(display (null? '()))       (newline)  ; #t
(display (pair? nums))      (newline)  ; #t
(display (number? 42))      (newline)  ; #t
```

## Higher-Order Functions

```scheme
; map
(display (map (lambda (x) (* x x)) '(1 2 3 4 5)))
(newline)

; filter (keeps elements where predicate is true)
(define (even? n) (= (remainder n 2) 0))
(display (filter even? '(1 2 3 4 5 6 7 8)))
(newline)

; fold-left (reduce)
(define (fold-left f init lst)
  (if (null? lst)
      init
      (fold-left f (f init (car lst)) (cdr lst))))

(display (fold-left + 0 '(1 2 3 4 5)))  ; 15
(newline)
```

## Let and Lambda

```scheme
; let — local bindings
(let ((a 3)
      (b 4))
  (display (+ a b)) (newline))      ; 7

; let* — sequential bindings
(let* ((x 2)
       (y (* x 3))
       (z (+ x y)))
  (display z) (newline))             ; 8

; lambda — anonymous function
(define add (lambda (x y) (+ x y)))
(display (add 10 20)) (newline)
```

## Tail Recursion

```scheme
; Tail-recursive factorial (Scheme guarantees TCO)
(define (fact-iter n acc)
  (if (= n 0)
      acc
      (fact-iter (- n 1) (* n acc))))

(define (factorial n) (fact-iter n 1))
(display (factorial 20)) (newline)
```

## Further Reading

- [Examples/scheme/](../Examples/scheme/) — 5 Scheme example programs
- [Language Guide: Scheme](LANGUAGE_GUIDE.md#scheme)
