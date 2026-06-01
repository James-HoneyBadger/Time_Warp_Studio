;;; Number Theory in LISP/Scheme
;;; Primes, GCD, factorization, modular arithmetic

;;; ── Prime testing ─────────────────────────────────────────────────────
(define (prime? n)
  (define (trial-div i)
    (cond ((> (* i i) n) #t)
          ((= (modulo n i) 0) #f)
          (else (trial-div (+ i 1)))))
  (if (< n 2) #f (trial-div 2)))

;;; Generate primes up to limit (Sieve of Eratosthenes)
(define (sieve limit)
  (let ((composite (make-vector (+ limit 1) #f)))
    (let loop ((i 2) (primes '()))
      (if (> i limit)
          (reverse primes)
          (if (vector-ref composite i)
              (loop (+ i 1) primes)
              (begin
                (let mark ((j (* i i)))
                  (when (<= j limit)
                    (vector-set! composite j #t)
                    (mark (+ j i))))
                (loop (+ i 1) (cons i primes))))))))

;;; ── GCD and LCM ──────────────────────────────────────────────────────
(define (my-gcd a b)
  (if (= b 0) a (my-gcd b (modulo a b))))

(define (my-lcm a b)
  (/ (* a b) (my-gcd a b)))

;;; ── Prime factorization ──────────────────────────────────────────────
(define (factorize n)
  (let loop ((n n) (d 2) (factors '()))
    (cond ((> (* d d) n)
           (if (> n 1)
               (reverse (cons n factors))
               (reverse factors)))
          ((= (modulo n d) 0)
           (loop (/ n d) d (cons d factors)))
          (else
           (loop n (+ d 1) factors)))))

;;; ── Euler's totient ──────────────────────────────────────────────────
(define (totient n)
  (length (filter (lambda (k) (= (my-gcd k n) 1))
                  (cdr (iota n)))))

(define (iota n)
  (let loop ((i 0) (acc '()))
    (if (= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

;;; ── Modular exponentiation ───────────────────────────────────────────
(define (mod-exp base exp mod)
  (cond ((= exp 0) 1)
        ((even? exp)
         (modulo (* (mod-exp base (/ exp 2) mod)
                    (mod-exp base (/ exp 2) mod)) mod))
        (else
         (modulo (* base (mod-exp base (- exp 1) mod)) mod))))

;;; ── Output ───────────────────────────────────────────────────────────
(display "=== Number Theory in LISP/Scheme ===") (newline)

(display "\nPrimes up to 50: ")
(display (sieve 50)) (newline)

(display "\nFactorizations:") (newline)
(for-each (lambda (n)
  (display "  ") (display n) (display " = ")
  (display (factorize n)) (newline))
  '(12 60 100 360 1024 2310))

(display "\nGCD examples:") (newline)
(for-each (lambda (pair)
  (display "  gcd(") (display (car pair)) (display ",")
  (display (cadr pair)) (display ") = ")
  (display (my-gcd (car pair) (cadr pair))) (newline))
  '((12 8) (100 75) (17 13) (0 5)))

(display "\nmod-exp(2, 10, 1000) = ")
(display (mod-exp 2 10 1000)) (newline)
