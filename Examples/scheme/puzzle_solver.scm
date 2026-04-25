; ============================================================
; PUZZLE SOLVER — Scheme Language Showcase
; 8-Queens * Towers of Hanoi * Y Combinator * Streams
; Time Warp Studio — Scheme Language Demo
; ============================================================

; ===== SECTION 1: UTILITY HELPERS =====

(define (display-line str)
  (display str)
  (newline))

(define (repeat-char ch n)
  (if (<= n 0) ""
      (string-append (string ch) (repeat-char ch (- n 1)))))

(define (divider) (display-line (repeat-char #\= 60)))

(define (section-header n title)
  (newline)
  (display (string-append "[ " (number->string n) " ] " title))
  (newline))

; ===== SECTION 2: 8-QUEENS SOLVER =====
; Complete backtracking solution — find all 92 solutions

(define (queens n)
  ; Returns list of all valid queen placements on n×n board
  ; Each placement is a list of column positions (1-indexed)
  (define (safe? col queens-so-far)
    (define (check row-diff q-list)
      (if (null? q-list) #t
          (let ((c (car q-list)))
            (and (not (= c col))
                 (not (= c (+ col row-diff)))
                 (not (= c (- col row-diff)))
                 (check (+ row-diff 1) (cdr q-list))))))
    (check 1 queens-so-far))

  (define (place-queens board-size current-row queens-so-far)
    (if (> current-row board-size)
        (list queens-so-far)     ; Found a solution
        (apply append
               (map (lambda (col)
                      (if (safe? col queens-so-far)
                          (place-queens board-size
                                        (+ current-row 1)
                                        (append queens-so-far (list col)))
                          '()))
                    (cdr (iota (+ board-size 1)))))))   ; cols 1..n

  (place-queens n 1 '()))

; iota helper (not always built-in in R5RS)
(define (iota n)
  (define (loop i acc)
    (if (< i 0) acc
        (loop (- i 1) (cons i acc))))
  (loop (- n 1) '()))

(define (display-board solution)
  ; Display one 8-queens solution as ASCII board
  (let ((n (length solution)))
    (for-each (lambda (col)
                (let ((row-str
                       (apply string-append
                              (map (lambda (c)
                                     (if (= c col) " Q " " . "))
                                   (cdr (iota (+ n 1)))))))
                  (display "    ")
                  (display-line row-str)))
              solution)
    (newline)))

; ===== SECTION 3: TOWERS OF HANOI =====

(define hanoi-moves 0)

(define (hanoi n from to via)
  (when (> n 0)
    (hanoi (- n 1) from via to)
    (set! hanoi-moves (+ hanoi-moves 1))
    (display "    Move disk ")
    (display n)
    (display " from peg ")
    (display from)
    (display " to peg ")
    (display to)
    (newline)
    (hanoi (- n 1) via to from)))

(define (hanoi-stats disks)
  (set! hanoi-moves 0)
  (display "  Towers of Hanoi with ")
  (display disks)
  (display-line " disks:")
  (if (<= disks 4)
      (hanoi disks 'A 'C 'B)
      (begin
        (display "  (Showing move count only for ")
        (display disks)
        (display-line " disks)")
        ; Compute 2^n - 1 without actually executing
        (set! hanoi-moves (- (expt 2 disks) 1))))
  (display "  Total moves: ")
  (display hanoi-moves)
  (display " (formula: 2^")
  (display disks)
  (display " - 1 = ")
  (display (- (expt 2 disks) 1))
  (display-line ")"))

; ===== SECTION 4: Y COMBINATOR =====
; The Y combinator — computes fixed points of higher-order functions
; Enables recursion without naming the function

(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

; Factorial via Y combinator
(define fact-Y
  (Y (lambda (self)
       (lambda (n)
         (if (<= n 1) 1
             (* n (self (- n 1))))))))

; Fibonacci via Y combinator
(define fib-Y
  (Y (lambda (self)
       (lambda (n)
         (if (< n 2) n
             (+ (self (- n 1)) (self (- n 2))))))))

; ===== SECTION 5: LAZY STREAMS =====
; Infinite streams using closures (delay/force pattern)

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t) (cons h (lambda () t)))))

(define (stream-car s) (car s))
(define (stream-cdr s) ((cdr s)))
(define stream-null '())
(define (stream-null? s) (null? s))

(define (stream-take n s)
  (if (or (= n 0) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (- n 1) (stream-cdr s)))))

(define (stream-map f s)
  (if (stream-null? s) stream-null
      (cons-stream (f (stream-car s))
                   (stream-map f (stream-cdr s)))))

(define (stream-filter pred s)
  (cond ((stream-null? s) stream-null)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

; Infinite integers
(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))

; Infinite Fibonacci stream
(define fibs
  (let loop ((a 0) (b 1))
    (cons-stream a (loop b (+ a b)))))

; Sieve of Eratosthenes as a stream
(define (sieve s)
  (cons-stream
   (stream-car s)
   (sieve (stream-filter
           (lambda (x) (not (= 0 (modulo x (stream-car s)))))
           (stream-cdr s)))))

(define primes-stream (sieve (integers-from 2)))

; ===== SECTION 6: CONTINUATION-PASSING STYLE =====

; CPS factorial
(define (fact-cps n k)
  (if (<= n 1)
      (k 1)
      (fact-cps (- n 1)
                (lambda (result) (k (* n result))))))

; CPS Fibonacci
(define (fib-cps n k)
  (if (< n 2)
      (k n)
      (fib-cps (- n 1)
               (lambda (r1)
                 (fib-cps (- n 2)
                          (lambda (r2) (k (+ r1 r2))))))))

; call/cc for early exit (find first element matching predicate)
(define (first-match pred lst)
  (call-with-current-continuation
   (lambda (return)
     (for-each (lambda (x)
                 (when (pred x) (return x)))
               lst)
     #f)))

; ===== SECTION 7: TREE OPERATIONS =====

; Binary search tree as nested lists (val left right)
(define (bst-insert tree val)
  (cond ((null? tree) (list val '() '()))
        ((< val (car tree))
         (list (car tree) (bst-insert (cadr tree) val) (caddr tree)))
        ((> val (car tree))
         (list (car tree) (cadr tree) (bst-insert (caddr tree) val)))
        (else tree)))  ; duplicate

(define (bst-inorder tree)
  (if (null? tree) '()
      (append (bst-inorder (cadr tree))
              (list (car tree))
              (bst-inorder (caddr tree)))))

(define (bst-height tree)
  (if (null? tree) 0
      (+ 1 (max (bst-height (cadr tree))
                 (bst-height (caddr tree))))))

(define (bst-contains? tree val)
  (cond ((null? tree) #f)
        ((= val (car tree)) #t)
        ((< val (car tree)) (bst-contains? (cadr tree) val))
        (else               (bst-contains? (caddr tree) val))))

; ===== MAIN PROGRAM =====

(divider)
(display-line "  PUZZLE SOLVER — Scheme Functional Showcase")
(display-line "  8-Queens | Hanoi | Y-Combinator | Streams | CPS")
(divider)

; ---- Section 1: 8-Queens ----
(section-header 1 "8-QUEENS PROBLEM (Backtracking)")
(let ((solutions (queens 8)))
  (display "  Total solutions for 8×8 board: ")
  (display-line (number->string (length solutions)))
  (display-line "  First solution board:")
  (display-board (car solutions))
  (display-line "  Last (92nd) solution board:")
  (display-board (list-ref solutions 91))
  ; Count symmetric solutions
  (display "  Solutions for 4-queens: ")
  (display-line (number->string (length (queens 4)))))

; ---- Section 2: Towers of Hanoi ----
(section-header 2 "TOWERS OF HANOI")
(hanoi-stats 3)
(newline)
(hanoi-stats 4)
(newline)
(hanoi-stats 10)
(newline)
(hanoi-stats 20)

; ---- Section 3: Y Combinator ----
(section-header 3 "Y COMBINATOR — Recursion Without Names")
(display-line "  Factorial via Y combinator:")
(for-each (lambda (n)
            (display "    ")
            (display n)
            (display "! = ")
            (display-line (number->string (fact-Y n))))
          '(0 1 2 3 4 5 6 7 8 9 10))
(display-line "  Fibonacci via Y combinator (F0..F12):")
(display "    ")
(for-each (lambda (n) (display (fib-Y n)) (display " ")) (iota 13))
(newline)

; ---- Section 4: Infinite Streams ----
(section-header 4 "INFINITE STREAMS (Lazy Evaluation)")
(display "  First 15 primes:    ")
(display-line (number->string
               (length (stream-take 15 primes-stream))))
(display "    ")
(for-each (lambda (p) (display p) (display " "))
          (stream-take 15 primes-stream))
(newline)
(display "  First 15 Fibonacci: ")
(display-line "")
(display "    ")
(for-each (lambda (f) (display f) (display " "))
          (stream-take 15 fibs))
(newline)
(display "  1000th prime:  ")
(display-line (number->string (car (stream-take 1 (stream-filter
                                                    (lambda (x) (>= x 1))
                                                    (stream-map
                                                     (lambda (x) x)
                                                     (let loop ((s primes-stream) (n 1000))
                                                       (if (= n 1) s (loop (stream-cdr s) (- n 1)))))))))  )

; ---- Section 5: CPS ----
(section-header 5 "CONTINUATION-PASSING STYLE")
(display "  10! via CPS: ")
(fact-cps 10 (lambda (r) (display-line (number->string r))))
(display "  F(10) via CPS: ")
(fib-cps 10 (lambda (r) (display-line (number->string r))))
(display "  First even > 10 in [1..20]: ")
(display-line (number->string
               (first-match (lambda (x) (and (even? x) (> x 10)))
                            '(1 2 3 4 5 6 7 8 9 10 11 12 13))))
(display "  First prime > 100 in stream: ")
(let ((p100 (stream-take 1 (stream-filter (lambda (x) (> x 100)) primes-stream))))
  (display-line (number->string (car p100))))

; ---- Section 6: BST ----
(section-header 6 "BINARY SEARCH TREE")
(let* ((vals '(50 30 70 20 40 60 80 10 25 35 45))
       (tree (fold-right bst-insert '() vals)))
  (display "  Inserted: ")
  (display-line (number->string (length vals)) )
  (display "  In-order: ")
  (display-line (apply string-append
                       (map (lambda (v) (string-append (number->string v) " "))
                            (bst-inorder tree))))
  (display "  Height: ")
  (display-line (number->string (bst-height tree)))
  (display "  Contains 40? ")
  (display-line (if (bst-contains? tree 40) "yes" "no"))
  (display "  Contains 55? ")
  (display-line (if (bst-contains? tree 55) "yes" "no")))

(newline)
(divider)
(display-line "  Scheme Puzzle Solver Complete!")
(display-line "  Closures | Tail calls | call/cc | Lazy streams | HOF")
(divider)
