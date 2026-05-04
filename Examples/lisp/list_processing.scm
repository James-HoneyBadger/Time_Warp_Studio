;;; List Processing in Scheme
;;; Demonstrates: map, filter, fold, quasiquote, association lists, sorting

(display "=== List Processing ===") (newline)

;; --- Basic List Operations ---
(display "--- Building Lists ---") (newline)
(define nums (list 1 2 3 4 5 6 7 8 9 10))
(display "nums: ") (display nums) (newline)
(display "car:  ") (display (car nums)) (newline)
(display "cdr:  ") (display (cdr nums)) (newline)
(display "cadr: ") (display (cadr nums)) (newline)

;; --- Map ---
(display "--- Map ---") (newline)
(define squares (map (lambda (x) (* x x)) nums))
(display "squares: ") (display squares) (newline)

(define evens-doubled
  (map (lambda (x) (* 2 x))
       (filter even? nums)))
(display "even*2:  ") (display evens-doubled) (newline)

;; --- Filter ---
(display "--- Filter ---") (newline)
(define big (filter (lambda (x) (> x 5)) nums))
(display "x > 5:   ") (display big) (newline)

(define (divisible? n d) (= (remainder n d) 0))
(define threes (filter (lambda (x) (divisible? x 3)) nums))
(display "div by 3: ") (display threes) (newline)

;; --- Fold ---
(display "--- Fold / Reduce ---") (newline)
(define total (fold-left + 0 nums))
(display "sum 1..10 = ") (display total) (newline)

(define product (fold-left * 1 (list 1 2 3 4 5)))
(display "5! via fold = ") (display product) (newline)

(define my-max
  (fold-left (lambda (acc x) (if (> x acc) x acc)) (car nums) (cdr nums)))
(display "max = ") (display my-max) (newline)

;; --- Quasiquote / List Construction ---
(display "--- Quasiquote ---") (newline)
(define person "Alice")
(define age 30)
(define info `(name ,person age ,age hobbies ,(list "Scheme" "Math" "Music")))
(display info) (newline)

;; --- Association Lists ---
(display "--- Association Lists ---") (newline)
(define phonebook
  '((alice . "555-1234")
    (bob   . "555-5678")
    (carol . "555-9012")))

(define (lookup name book)
  (let ((entry (assq name book)))
    (if entry (cdr entry) "not found")))

(display "alice: ") (display (lookup 'alice phonebook)) (newline)
(display "bob:   ") (display (lookup 'bob   phonebook)) (newline)
(display "dave:  ") (display (lookup 'dave  phonebook)) (newline)

;; --- Sorting ---
(display "--- Sorting ---") (newline)
(define unsorted '(5 2 8 1 9 3 7 4 6))
(display "unsorted: ") (display unsorted) (newline)
(define sorted (sort unsorted <))
(display "sorted:   ") (display sorted) (newline)

;; --- Higher-order: compose and pipeline ---
(display "--- Function Composition ---") (newline)
(define (compose f g) (lambda (x) (f (g x))))
(define add1 (lambda (x) (+ x 1)))
(define double (lambda (x) (* x 2)))
(define double-then-add1 (compose add1 double))
(display "double-then-add1(5) = ") (display (double-then-add1 5)) (newline)

;; Pipeline: filter -> map -> fold
(define result
  (fold-left +
             0
             (map (lambda (x) (* x x))
                  (filter odd? (list 1 2 3 4 5 6 7 8 9 10)))))
(display "sum of squares of odd 1..10 = ") (display result) (newline)

;; --- Named let (loop pattern) ---
(display "--- Named Let Loop ---") (newline)
(define (range start stop)
  (let loop ((i start) (acc '()))
    (if (>= i stop)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

(display (range 1 11)) (newline)
