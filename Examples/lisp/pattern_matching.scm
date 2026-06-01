;;; Pattern Matching and Association Lists in LISP/Scheme
;;; Demonstrates alist operations, lookup tables, and simple dispatch

;;; ── Association lists (dictionaries) ─────────────────────────────────
(define phonebook
  '((alice . "555-1234")
    (bob   . "555-5678")
    (carol . "555-9012")
    (dave  . "555-3456")))

(define (lookup key alist)
  (let ((pair (assq key alist)))
    (if pair (cdr pair) #f)))

(define (insert key val alist)
  (cons (cons key val)
        (filter (lambda (pair) (not (eq? (car pair) key))) alist)))

(define (keys alist) (map car alist))
(define (values alist) (map cdr alist))

;;; ── Dispatch table (method lookup) ──────────────────────────────────
(define (make-dispatch-table . entries)
  entries)

(define math-ops
  (make-dispatch-table
    (cons '+ (lambda (a b) (+ a b)))
    (cons '- (lambda (a b) (- a b)))
    (cons '* (lambda (a b) (* a b)))
    (cons '/ (lambda (a b) (if (= b 0) 'error (/ a b))))))

(define (dispatch table op . args)
  (let ((fn (lookup op table)))
    (if fn
        (apply fn args)
        (error "Unknown operation" op))))

;;; ── Simple pattern matcher ───────────────────────────────────────────
(define (match pattern data)
  (cond
    ((and (null? pattern) (null? data)) '())
    ((or (null? pattern) (null? data)) #f)
    ((eq? (car pattern) '?)
     (let ((rest (match (cdr pattern) (cdr data))))
       (if rest (cons (car data) rest) #f)))
    ((equal? (car pattern) (car data))
     (match (cdr pattern) (cdr data)))
    (else #f)))

;;; ── Output ───────────────────────────────────────────────────────────
(display "=== Association Lists ===") (newline)
(display "Phonebook: ") (display phonebook) (newline)
(display "Alice's number: ") (display (lookup 'alice phonebook)) (newline)
(display "Unknown: ") (display (lookup 'eve phonebook)) (newline)

(let ((updated (insert 'eve "555-7890" phonebook)))
  (display "After adding Eve: ") (display (lookup 'eve updated)) (newline))

(newline)
(display "=== Dispatch Table ===") (newline)
(for-each
  (lambda (expr)
    (display "  ") (display expr) (display " = ")
    (display (dispatch math-ops (car expr) (cadr expr) (caddr expr)))
    (newline))
  '((+ 10 5) (- 10 5) (* 10 5) (/ 10 5) (/ 10 0)))

(newline)
(display "=== Pattern Matching ===") (newline)
(define tests
  (list
    (list '(a ? c) '(a b c))
    (list '(? ?) '(1 2))
    (list '(a b c) '(a b c))
    (list '(a ? c) '(a b d))))
(for-each
  (lambda (t)
    (display "  pattern=") (display (car t))
    (display " data=") (display (cadr t))
    (display " => ")
    (display (match (car t) (cadr t)))
    (newline))
  tests)
