;;; ════════════════════════════════════════════════════════
;;;  SCHEME FUNCTIONAL TOOLKIT
;;;  A showcase of Scheme/Lisp functional programming:
;;;  higher-order functions, streams, pattern matching,
;;;  monadic computation, memoization, and more.
;;; ════════════════════════════════════════════════════════

;;; ─── BASIC HIGHER-ORDER FUNCTIONS ────────────────────────
(define (compose f g)
  (lambda (x) (f (g x))))

(define (curry f)
  (lambda (x) (lambda (y) (f x y))))

(define (flip f)
  (lambda (x y) (f y x)))

(define (identity x) x)
(define (const x) (lambda (_) x))

;;; ─── ARITHMETIC UTILITIES ─────────────────────────────────
(define (square x) (* x x))
(define (cube x)   (* x x x))
(define (average x y) (/ (+ x y) 2.0))
(define (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

(define (fibonacci n)
  (define (fib-iter a b count)
    (if (= count 0) b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))

(define (lcm a b)
  (/ (* a b) (gcd a b)))

;;; ─── LIST UTILITIES ───────────────────────────────────────
(define (flatten lst)
  (cond ((null? lst) '())
        ((pair? (car lst))
         (append (flatten (car lst)) (flatten (cdr lst))))
        (else (cons (car lst) (flatten (cdr lst))))))

(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (list (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

(define (take n lst)
  (if (or (= n 0) (null? lst)) '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (drop n lst)
  (if (or (= n 0) (null? lst)) lst
      (drop (- n 1) (cdr lst))))

(define (range start end)
  (if (>= start end) '()
      (cons start (range (+ start 1) end))))

(define (sum lst) (fold-left + 0 lst))
(define (product lst) (fold-left * 1 lst))
(define (maximum lst)
  (fold-left (lambda (acc x) (if (> x acc) x acc)) (car lst) (cdr lst)))
(define (minimum lst)
  (fold-left (lambda (acc x) (if (< x acc) x acc)) (car lst) (cdr lst)))

(define (count pred lst)
  (fold-left (lambda (acc x) (if (pred x) (+ acc 1) acc)) 0 lst))

(define (group-by key lst)
  (fold-left
    (lambda (acc x)
      (let* ((k (key x))
             (group (assoc k acc)))
        (if group
            (begin (set-cdr! group (cons x (cdr group))) acc)
            (cons (list k x) acc))))
    '() lst))

;;; ─── SORTING ──────────────────────────────────────────────
(define (quicksort lst)
  (if (or (null? lst) (null? (cdr lst))) lst
      (let ((pivot (car lst))
            (rest  (cdr lst)))
        (append
          (quicksort (filter (lambda (x) (< x pivot)) rest))
          (list pivot)
          (quicksort (filter (lambda (x) (>= x pivot)) rest))))))

(define (mergesort lst)
  (define (merge a b)
    (cond ((null? a) b)
          ((null? b) a)
          ((< (car a) (car b))
           (cons (car a) (merge (cdr a) b)))
          (else
           (cons (car b) (merge a (cdr b))))))
  (define (split lst)
    (if (or (null? lst) (null? (cdr lst)))
        (values lst '())
        (let-values (((s1 s2) (split (cddr lst))))
          (values (cons (car lst) s1)
                  (cons (cadr lst) s2)))))
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let-values (((left right) (split lst)))
        (merge (mergesort left) (mergesort right)))))

;;; ─── MEMOIZATION ──────────────────────────────────────────
(define (memoize f)
  (let ((cache '()))
    (lambda args
      (let ((cached (assoc args cache)))
        (if cached
            (cdr cached)
            (let ((result (apply f args)))
              (set! cache (cons (cons args result) cache))
              result))))))

(define fib-memo
  (memoize
    (lambda (n)
      (if (< n 2) n
          (+ (fib-memo (- n 1)) (fib-memo (- n 2)))))))

;;; ─── STREAMS (LAZY LISTS) ─────────────────────────────────
(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons x s) (cons x (delay s)))))

(define stream-car car)
(define (stream-cdr s) (force (cdr s)))
(define stream-null? null?)
(define the-empty-stream '())

(define (stream-take n s)
  (if (or (= n 0) (stream-null? s)) '()
      (cons (stream-car s)
            (stream-take (- n 1) (stream-cdr s)))))

(define (stream-map f s)
  (if (stream-null? s) the-empty-stream
      (stream-cons (f (stream-car s))
                   (stream-map f (stream-cdr s)))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (stream-cons (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

;; Infinite stream of naturals
(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

(define naturals (integers-from 0))
(define positives (integers-from 1))

;; Sieve of Eratosthenes on streams
(define (sieve s)
  (let ((p (stream-car s)))
    (stream-cons p
      (sieve (stream-filter
               (lambda (x) (not (= 0 (remainder x p))))
               (stream-cdr s))))))

(define primes (sieve (integers-from 2)))

;;; ─── TREE OPERATIONS ─────────────────────────────────────
(define (make-tree val left right) (list val left right))
(define tree-val  car)
(define tree-left cadr)
(define tree-right caddr)
(define (leaf? t) (and (null? (tree-left t)) (null? (tree-right t))))

(define (bst-insert t val)
  (cond ((null? t) (make-tree val '() '()))
        ((< val (tree-val t))
         (make-tree (tree-val t) (bst-insert (tree-left t) val) (tree-right t)))
        ((> val (tree-val t))
         (make-tree (tree-val t) (tree-left t) (bst-insert (tree-right t) val)))
        (else t)))

(define (bst-contains? t val)
  (cond ((null? t) #f)
        ((= val (tree-val t)) #t)
        ((< val (tree-val t)) (bst-contains? (tree-left t) val))
        (else (bst-contains? (tree-right t) val))))

(define (tree->list t)
  (if (null? t) '()
      (append (tree->list (tree-left t))
              (list (tree-val t))
              (tree->list (tree-right t)))))

;;; ─── STATISTICS ───────────────────────────────────────────
(define (mean lst)
  (/ (sum lst) (length lst)))

(define (variance lst)
  (let* ((m (mean lst))
         (diffs (map (lambda (x) (square (- x m))) lst)))
    (/ (sum diffs) (length lst))))

(define (std-dev lst)
  (sqrt (variance lst)))

;;; ─── DEMO ─────────────────────────────────────────────────
(define (println . args)
  (for-each display args) (newline))

(define (demo-section title)
  (newline)
  (println "  ─── " title " ───"))

(println "╔════════════════════════════════════════╗")
(println "║     SCHEME FUNCTIONAL TOOLKIT          ║")
(println "╚════════════════════════════════════════╝")

(demo-section "HIGHER-ORDER FUNCTIONS")
(let ((add-5 ((curry +) 5))
      (double (lambda (x) (* x 2)))
      (double-then-add5 (compose ((curry +) 5) (lambda (x) (* x 2)))))
  (println "  (add-5 10) = " (add-5 10))
  (println "  (double 7) = " (double 7))
  (println "  (double-then-add5 3) = " (double-then-add5 3)))

(demo-section "LIST OPERATIONS")
(let* ((nums (range 1 11))
       (evens (filter even? nums))
       (squares (map square nums))
       (total (sum nums)))
  (println "  range(1,11): " nums)
  (println "  evens: " evens)
  (println "  squares: " squares)
  (println "  sum: " total)
  (println "  max: " (maximum nums) "  min: " (minimum nums)))

(demo-section "FLATTENING & ZIPPING")
(let ((nested '(1 (2 3) (4 (5 6)) 7)))
  (println "  flatten " nested " = " (flatten nested)))
(let ((z (zip '(a b c) '(1 2 3))))
  (println "  zip (a b c)(1 2 3) = " z))

(demo-section "SORTING")
(let* ((unsorted '(5 1 9 3 7 2 8 4 6))
       (qs (quicksort unsorted))
       (ms (mergesort unsorted)))
  (println "  unsorted: " unsorted)
  (println "  quicksort: " qs)
  (println "  mergesort: " ms))

(demo-section "MEMOIZED FIBONACCI")
(println "  fib(0..9): " (map fib-memo (range 0 10)))
(println "  fib(30) = " (fib-memo 30))
(println "  fib(40) = " (fib-memo 40))

(demo-section "LAZY STREAMS - First 10 primes")
(println "  primes: " (stream-take 10 primes))
(println "  first 10 squares: "
  (stream-take 10 (stream-map square positives)))

(demo-section "BINARY SEARCH TREE")
(let* ((vals '(5 3 7 1 4 6 8 2))
       (bst (fold-left bst-insert '() vals))
       (sorted (tree->list bst)))
  (println "  inserted: " vals)
  (println "  in-order: " sorted)
  (println "  contains 4? " (bst-contains? bst 4))
  (println "  contains 9? " (bst-contains? bst 9)))

(demo-section "STATISTICS")
(let* ((data '(4 7 13 16 21 3 9 11 15 8))
       (m (mean data))
       (s (std-dev data)))
  (println "  data: " data)
  (println "  mean: " m)
  (println "  std-dev: " s))

(println)
(println "  ✓ Scheme functional toolkit complete!")
