;;; Higher-Order Functions in LISP/Scheme
;;; map, filter, reduce, compose, curry, memoize

;;; ── Core higher-order functions ──────────────────────────────────────

;;; Function composition
(define (compose . fns)
  (if (null? fns)
      (lambda (x) x)
      (let ((fn (car fns))
            (rest (apply compose (cdr fns))))
        (lambda (x) (fn (rest x))))))

;;; Curry: convert f(a,b) to f(a)(b)
(define (curry f)
  (lambda (x) (lambda (y) (f x y))))

;;; Partial application
(define (partial f . args)
  (lambda rest
    (apply f (append args rest))))

;;; Memoize
(define (memoize f)
  (let ((cache '()))
    (lambda args
      (let ((cached (assoc args cache)))
        (if cached
            (cdr cached)
            (let ((result (apply f args)))
              (set! cache (cons (cons args result) cache))
              result))))))

;;; ── Using built-in higher-order functions ────────────────────────────

(define numbers '(1 2 3 4 5 6 7 8 9 10))

;;; map
(define squares (map (lambda (x) (* x x)) numbers))

;;; filter
(define evens (filter even? numbers))
(define odds  (filter odd? numbers))

;;; fold/reduce
(define (my-reduce f init lst)
  (if (null? lst)
      init
      (my-reduce f (f init (car lst)) (cdr lst))))

(define total (my-reduce + 0 numbers))
(define product (my-reduce * 1 numbers))

;;; ── Compose demo ─────────────────────────────────────────────────────
(define add1    (lambda (x) (+ x 1)))
(define double  (lambda (x) (* x 2)))
(define square  (lambda (x) (* x x)))

(define double-then-add1 (compose add1 double))
(define square-then-double (compose double square))
(define triple-compose (compose add1 double add1))

;;; ── Curry demo ───────────────────────────────────────────────────────
(define curried-add (curry +))
(define add5 (curried-add 5))
(define add10 (curried-add 10))

;;; ── Memoized Fibonacci ───────────────────────────────────────────────
(define fib
  (memoize
    (lambda (n)
      (if (< n 2)
          n
          (+ (fib (- n 1)) (fib (- n 2)))))))

;;; ── Output ───────────────────────────────────────────────────────────
(display "=== Higher-Order Functions in LISP/Scheme ===") (newline)

(display "Numbers: ") (display numbers) (newline)
(display "Squares: ") (display squares) (newline)
(display "Evens:   ") (display evens)   (newline)
(display "Odds:    ") (display odds)    (newline)
(display "Sum:     ") (display total)   (newline)
(display "Product: ") (display product) (newline)

(newline)
(display "=== Function Composition ===") (newline)
(display "double-then-add1(5) = ") (display (double-then-add1 5)) (newline)
(display "square-then-double(4) = ") (display (square-then-double 4)) (newline)
(display "triple-compose(3) = ") (display (triple-compose 3)) (newline)

(newline)
(display "=== Curry ===") (newline)
(display "add5(7) = ")  (display (add5 7))  (newline)
(display "add10(7) = ") (display (add10 7)) (newline)

(newline)
(display "=== Memoized Fibonacci ===") (newline)
(display "fib(10) = ") (display (fib 10)) (newline)
(display "fib(20) = ") (display (fib 20)) (newline)
(display "fib(30) = ") (display (fib 30)) (newline)
