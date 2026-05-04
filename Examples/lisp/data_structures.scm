;;; Data Structures in Scheme
;;; Demonstrates: closures as objects, vectors, association lists, records

(display "=== Data Structures in Scheme ===") (newline)

;; --- Stack using closures ---
(display "--- Closure-based Stack ---") (newline)

(define (make-stack)
  (let ((data '()))
    (lambda (msg . args)
      (cond
        ((eq? msg 'push)  (set! data (cons (car args) data)))
        ((eq? msg 'pop)   (if (null? data)
                              (error "stack underflow")
                              (let ((top (car data)))
                                (set! data (cdr data))
                                top)))
        ((eq? msg 'peek)  (if (null? data) (error "empty stack") (car data)))
        ((eq? msg 'empty?) (null? data))
        ((eq? msg 'size)  (length data))
        (else (error "unknown message" msg))))))

(define s (make-stack))
(s 'push 10)
(s 'push 20)
(s 'push 30)
(display "size: ")   (display (s 'size))  (newline)
(display "peek: ")   (display (s 'peek))  (newline)
(display "pop: ")    (display (s 'pop))   (newline)
(display "pop: ")    (display (s 'pop))   (newline)
(display "size: ")   (display (s 'size))  (newline)

;; --- Queue using two stacks ---
(display "--- Queue ---") (newline)

(define (make-queue)
  (let ((inbox '()) (outbox '()))
    (define (transfer!)
      (when (null? outbox)
        (set! outbox (reverse inbox))
        (set! inbox '())))
    (lambda (msg . args)
      (cond
        ((eq? msg 'enqueue) (set! inbox (cons (car args) inbox)))
        ((eq? msg 'dequeue) (transfer!)
                            (if (null? outbox)
                                (error "empty queue")
                                (let ((front (car outbox)))
                                  (set! outbox (cdr outbox))
                                  front)))
        ((eq? msg 'empty?)  (and (null? inbox) (null? outbox)))
        ((eq? msg 'size)    (+ (length inbox) (length outbox)))
        (else (error "unknown message" msg))))))

(define q (make-queue))
(q 'enqueue "first")
(q 'enqueue "second")
(q 'enqueue "third")
(display "dequeue: ") (display (q 'dequeue)) (newline)
(q 'enqueue "fourth")
(display "dequeue: ") (display (q 'dequeue)) (newline)
(display "size: ")    (display (q 'size))    (newline)

;; --- Vectors ---
(display "--- Vectors ---") (newline)

(define v (make-vector 5 0))
(let loop ((i 0))
  (when (< i 5)
    (vector-set! v i (* i i))
    (loop (+ i 1))))
(display "squares vector: ") (display v) (newline)
(display "v[3] = ")          (display (vector-ref v 3)) (newline)

;; --- Hash table ---
(display "--- Hash Table ---") (newline)

(define ht (make-hash-table))
(hash-table-set! ht 'name  "Alice")
(hash-table-set! ht 'age   30)
(hash-table-set! ht 'skill "Scheme")

(display "name:  ") (display (hash-table-ref ht 'name))  (newline)
(display "age:   ") (display (hash-table-ref ht 'age))   (newline)
(display "skill: ") (display (hash-table-ref ht 'skill)) (newline)

;; --- Binary search tree ---
(display "--- Binary Search Tree ---") (newline)

;; BST node: (value left right)
(define (bst-make val left right) (list val left right))
(define (bst-val  node) (car node))
(define (bst-left node) (cadr node))
(define (bst-right node)(caddr node))
(define bst-empty '())
(define (bst-empty? t) (null? t))

(define (bst-insert t val)
  (cond
    ((bst-empty? t) (bst-make val bst-empty bst-empty))
    ((< val (bst-val t))
     (bst-make (bst-val t)
               (bst-insert (bst-left t) val)
               (bst-right t)))
    ((> val (bst-val t))
     (bst-make (bst-val t)
               (bst-left t)
               (bst-insert (bst-right t) val)))
    (else t)))  ; duplicate — ignore

(define (bst-inorder t)
  (if (bst-empty? t)
      '()
      (append (bst-inorder (bst-left t))
              (list (bst-val t))
              (bst-inorder (bst-right t)))))

(define tree bst-empty)
(for-each (lambda (n) (set! tree (bst-insert tree n)))
          '(5 3 7 1 4 6 8 2))

(display "in-order: ") (display (bst-inorder tree)) (newline)

;; --- Memoisation ---
(display "--- Memoisation ---") (newline)

(define (memoize f)
  (let ((cache (make-hash-table)))
    (lambda args
      (let ((key args))
        (let ((cached (hash-table-ref cache key #f)))
          (or cached
              (let ((result (apply f args)))
                (hash-table-set! cache key result)
                result)))))))

(define fib
  (memoize
    (lambda (n)
      (if (< n 2)
          n
          (+ (fib (- n 1)) (fib (- n 2)))))))

(display "fib(30) = ") (display (fib 30)) (newline)
(display "fib(35) = ") (display (fib 35)) (newline)
