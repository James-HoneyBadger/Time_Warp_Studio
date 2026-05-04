;;; Recursive Algorithms in Scheme
;;; Demonstrates: define, recursion, accumulator pattern, tail-call optimisation

;; --- Factorial ---
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(display "--- Factorial ---") (newline)
(display "5!  = ") (display (factorial 5))  (newline)
(display "10! = ") (display (factorial 10)) (newline)
(display "20! = ") (display (factorial 20)) (newline)

;; Tail-recursive factorial
(define (factorial-tail n acc)
  (if (<= n 1)
      acc
      (factorial-tail (- n 1) (* n acc))))

(display "20! (tail) = ") (display (factorial-tail 20 1)) (newline)

;; --- Fibonacci ---
(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))))

;; Tail-recursive Fibonacci
(define (fib-tail n a b)
  (if (= n 0)
      a
      (fib-tail (- n 1) b (+ a b))))

(define (fibonacci n) (fib-tail n 0 1))

(display "--- Fibonacci ---") (newline)
(display "First 12: ")
(let loop ((i 0))
  (when (< i 12)
    (display (fibonacci i))
    (display " ")
    (loop (+ i 1))))
(newline)

;; --- Power (fast exponentiation) ---
(define (power base exp)
  (cond
    ((= exp 0) 1)
    ((even? exp)
     (let ((half (power base (quotient exp 2))))
       (* half half)))
    (else (* base (power base (- exp 1))))))

(display "--- Powers ---") (newline)
(display "2^16 = ") (display (power 2 16)) (newline)
(display "3^10 = ") (display (power 3 10)) (newline)

;; --- GCD and LCM ---
(define (my-gcd a b)
  (if (= b 0) a (my-gcd b (remainder a b))))

(define (my-lcm a b)
  (quotient (* a b) (my-gcd a b)))

(display "--- GCD / LCM ---") (newline)
(display "gcd(48, 18) = ") (display (my-gcd 48 18)) (newline)
(display "lcm(4, 6)   = ") (display (my-lcm 4 6))   (newline)

;; --- Tower of Hanoi ---
(define hanoi-moves 0)

(define (hanoi n from to via)
  (when (> n 0)
    (hanoi (- n 1) from via to)
    (set! hanoi-moves (+ hanoi-moves 1))
    (hanoi (- n 1) via to from)))

(hanoi 6 'A 'C 'B)
(display "--- Tower of Hanoi ---") (newline)
(display "Moves for 6 discs: ") (display hanoi-moves) (newline)

;; --- Flatten nested list ---
(define (flatten lst)
  (cond
    ((null? lst) '())
    ((pair? (car lst))
     (append (flatten (car lst)) (flatten (cdr lst))))
    (else
     (cons (car lst) (flatten (cdr lst))))))

(display "--- Flatten ---") (newline)
(display (flatten '(1 (2 3) (4 (5 6)) 7))) (newline)
