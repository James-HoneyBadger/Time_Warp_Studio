;;; ============================================================
;;; LAMBDA CALCULUS — Scheme Language Showcase
;;; Beta reduction, Church numerals, Church booleans, Y combinator
;;; Time Warp Studio — Scheme Language Demo
;;; ============================================================

;;; ============================================================
;;; PART 1: CHURCH ENCODING OF BOOLEANS
;;; ============================================================

;;; Church true: λx.λy.x
(define church-true  (lambda (x) (lambda (y) x)))

;;; Church false: λx.λy.y
(define church-false (lambda (x) (lambda (y) y)))

;;; Decode a Church boolean to Scheme boolean
(define (church-bool->bool b) ((b #t) #f))

;;; Boolean operators
(define (church-and a b) ((a b) church-false))
(define (church-or  a b) ((a church-true) b))
(define (church-not a)   ((a church-false) church-true))
(define (church-if  c t f) ((c t) f))
(define (church-xor a b)   (church-and (church-or a b) (church-not (church-and a b))))

;;; ============================================================
;;; PART 2: CHURCH NUMERALS
;;; ============================================================

;;; Church zero: λf.λx.x
(define church-zero  (lambda (f) (lambda (x) x)))

;;; Successor: λn.λf.λx.f (n f x)
(define (church-succ n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;;; Convert integer to Church numeral
(define (int->church n)
  (if (= n 0) church-zero
      (church-succ (int->church (- n 1)))))

;;; Convert Church numeral to integer
(define (church->int n) ((n (lambda (x) (+ x 1))) 0))

;;; Church addition: λm.λn.λf.λx. m f (n f x)
(define (church-add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

;;; Church multiplication: λm.λn.λf. m (n f)
(define (church-mul m n)
  (lambda (f) (n (m f))))

;;; Church exponentiation: λm.λn. n m
(define (church-exp m n) (n m))

;;; Church predecessor (requires pairs trick)
;;; Predecessor: λn.λf.λx. n (λg.λh. h (g f)) (λu.x) (λu.u)
(define (church-pred n)
  (lambda (f)
    (lambda (x)
      (((n (lambda (g)
             (lambda (h)
               (h (g f)))))
        (lambda (u) x))
       (lambda (u) u)))))

;;; Church subtraction: m - n = apply pred n times to m
(define (church-sub m n)
  ((n church-pred) m))

;;; Is-zero predicate: λn. n (λx.false) true
(define (church-zero? n)
  ((n (lambda (x) church-false)) church-true))

;;; ============================================================
;;; PART 3: CHURCH PAIRS
;;; ============================================================

(define (church-pair a b) (lambda (f) ((f a) b)))
(define (church-fst p) (p church-true))
(define (church-snd p) (p church-false))

;;; ============================================================
;;; PART 4: Y COMBINATOR (Fixed-point combinator)
;;; ============================================================

;;; Applicative-order Y (call-by-value safe version: Z combinator)
(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

;;; Factorial via Y combinator
(define factorial
  (Y (lambda (self)
       (lambda (n)
         (if (= n 0) 1
             (* n (self (- n 1))))))))

;;; Fibonacci via Y combinator
(define fibonacci
  (Y (lambda (self)
       (lambda (n)
         (if (< n 2) n
             (+ (self (- n 1))
                (self (- n 2))))))))

;;; ============================================================
;;; PART 5: LAMBDA CALCULUS EVALUATOR
;;; Representation: variables as symbols, lambda as (λ x body),
;;;                 application as (app f arg)
;;; ============================================================

;;; Substitute all free occurrences of var in expr with value
(define (substitute expr var value)
  (cond
    [(symbol? expr)
     (if (eq? expr var) value expr)]
    [(and (pair? expr) (eq? (car expr) 'λ))
     (let* ([param (cadr expr)]
            [body  (caddr expr)])
       (if (eq? param var)
           expr  ; variable is shadowed by this lambda
           (list 'λ param (substitute body var value))))]
    [(and (pair? expr) (eq? (car expr) 'app))
     (list 'app
           (substitute (cadr expr)  var value)
           (substitute (caddr expr) var value))]
    [else expr]))

;;; Beta-reduce one step
(define (beta-reduce expr)
  (cond
    [(symbol? expr) #f]  ; atoms: no reduction
    [(and (pair? expr) (eq? (car expr) 'λ)) #f]  ; lambda: no reduction
    [(and (pair? expr) (eq? (car expr) 'app))
     (let* ([fn  (cadr expr)]
            [arg (caddr expr)])
       (cond
         [(and (pair? fn) (eq? (car fn) 'λ))
          ;; Beta reduction: (λx.body) arg => body[x:=arg]
          (substitute (caddr fn) (cadr fn) arg)]
         [else
          ;; Try to reduce the function
          (let ([reduced-fn (beta-reduce fn)])
            (if reduced-fn
                (list 'app reduced-fn arg)
                ;; Try to reduce the argument
                (let ([reduced-arg (beta-reduce arg)])
                  (if reduced-arg
                      (list 'app fn reduced-arg)
                      #f))))]))]
    [else #f]))

;;; Reduce to normal form (loop until no more reductions)
(define (normalize expr max-steps)
  (let loop ([e expr] [steps 0])
    (if (>= steps max-steps)
        (cons 'TIMEOUT e)
        (let ([reduced (beta-reduce e)])
          (if reduced
              (loop reduced (+ steps 1))
              (cons steps e))))))

;;; Pretty-print a lambda expression
(define (pp-lambda expr)
  (cond
    [(symbol? expr) (symbol->string expr)]
    [(and (pair? expr) (eq? (car expr) 'λ))
     (string-append "(λ" (symbol->string (cadr expr)) "." (pp-lambda (caddr expr)) ")")]
    [(and (pair? expr) (eq? (car expr) 'app))
     (string-append "(" (pp-lambda (cadr expr)) " " (pp-lambda (caddr expr)) ")")]
    [else (display expr) ""]))

;;; ============================================================
;;; PART 6: CHURCH-ENCODED LISTS
;;; ============================================================

;;; nil = λc.λn.n
(define church-nil (lambda (c) (lambda (n) n)))

;;; cons = λh.λt.λc.λn. c h (t c n)
(define (church-cons h t)
  (lambda (c) (lambda (n) ((c h) ((t c) n)))))

;;; Decode a Church list to Scheme list
(define (church-list->list lst)
  ((lst cons) '()))

;;; Fold left over Church list
(define (church-fold f init lst)
  ((lst f) init))

;;; Build a Church list from a Scheme list
(define (list->church-list lst)
  (if (null? lst)
      church-nil
      (church-cons (car lst) (list->church-list (cdr lst)))))

;;; ============================================================
;;; MAIN PROGRAM
;;; ============================================================

(define (display-section title)
  (newline)
  (display (make-string 60 #\─)) (newline)
  (display title) (newline)
  (display (make-string 60 #\─)) (newline))

(define (assert-equal label got expected)
  (display (string-append "  " label ": "))
  (display got)
  (if (equal? got expected)
      (display " ✓")
      (begin (display " ✗ expected: ") (display expected)))
  (newline))

(display "============================================================") (newline)
(display "  LAMBDA CALCULUS — Scheme Showcase") (newline)
(display "  Church encoding, beta reduction, Y combinator") (newline)
(display "============================================================") (newline)

;;; ----- Section 1: Church Booleans -----
(display-section "SECTION 1: CHURCH BOOLEANS")
(define T church-true)
(define F church-false)

(display "  Encoding: TRUE = λx.λy.x  |  FALSE = λx.λy.y") (newline)
(newline)
(assert-equal "TRUE"          (church-bool->bool T) #t)
(assert-equal "FALSE"         (church-bool->bool F) #f)
(assert-equal "AND T T"       (church-bool->bool (church-and T T)) #t)
(assert-equal "AND T F"       (church-bool->bool (church-and T F)) #f)
(assert-equal "OR  F T"       (church-bool->bool (church-or  F T)) #t)
(assert-equal "OR  F F"       (church-bool->bool (church-or  F F)) #f)
(assert-equal "NOT T"         (church-bool->bool (church-not T)) #f)
(assert-equal "NOT F"         (church-bool->bool (church-not F)) #t)
(assert-equal "XOR T F"       (church-bool->bool (church-xor T F)) #t)
(assert-equal "XOR T T"       (church-bool->bool (church-xor T T)) #f)
(assert-equal "IF T then 1"   (church-if T 1 2) 1)
(assert-equal "IF F then 2"   (church-if F 1 2) 2)

;;; ----- Section 2: Church Numerals -----
(display-section "SECTION 2: CHURCH NUMERALS")
(display "  Encoding: 0 = λf.λx.x  |  1 = λf.λx.f x  |  etc.") (newline)
(newline)

(define zero  (int->church 0))
(define one   (int->church 1))
(define two   (int->church 2))
(define three (int->church 3))
(define four  (int->church 4))
(define five  (int->church 5))

(display "  Church numeral to integer:") (newline)
(for-each (lambda (n)
  (assert-equal (string-append "church->int " (number->string n))
                (church->int (int->church n)) n))
  '(0 1 2 3 4 5 6 7 8 9 10))

(newline)
(display "  Arithmetic on Church numerals:") (newline)
(assert-equal "2 + 3"   (church->int (church-add two three))   5)
(assert-equal "3 + 4"   (church->int (church-add three four))  7)
(assert-equal "2 * 3"   (church->int (church-mul two three))   6)
(assert-equal "4 * 5"   (church->int (church-mul four five))   20)
(assert-equal "2 ^ 3"   (church->int (church-exp two three))   8)
(assert-equal "3 ^ 2"   (church->int (church-exp three two))   9)
(assert-equal "5 - 2"   (church->int (church-sub five two))    3)
(assert-equal "zero?"   (church-bool->bool (church-zero? zero)) #t)
(assert-equal "zero? 3" (church-bool->bool (church-zero? three)) #f)

;;; ----- Section 3: Church Pairs -----
(display-section "SECTION 3: CHURCH PAIRS")
(define p (church-pair 42 "hello"))
(assert-equal "fst pair" (church-fst p) 42)
(assert-equal "snd pair" (church-snd p) "hello")

;;; ----- Section 4: Y Combinator -----
(display-section "SECTION 4: Y COMBINATOR")
(display "  Y = λf. (λx. f (λv. x x v)) (λx. f (λv. x x v))") (newline)
(newline)
(display "  Factorial via Y combinator:") (newline)
(for-each (lambda (n)
  (assert-equal (string-append (number->string n) "!")
                (factorial n)
                (let fact ([x n]) (if (= x 0) 1 (* x (fact (- x 1)))))))
  '(0 1 2 3 4 5 6 7 8 9 10))

(newline)
(display "  Fibonacci via Y combinator:") (newline)
(for-each (lambda (n)
  (assert-equal (string-append "fib(" (number->string n) ")")
                (fibonacci n)
                (let fib ([x n]) (if (< x 2) x (+ (fib (- x 1)) (fib (- x 2)))))))
  '(0 1 2 3 4 5 6 7 8 9 10 11 12))

;;; ----- Section 5: Beta Reduction -----
(display-section "SECTION 5: BETA REDUCTION EXAMPLES")
(display "  Notation: (λx.body) arg => body[x:=arg]") (newline)
(newline)

(define examples
  (list
    (list "(λx.x) y" '(app (λ x x) y))                        ; identity
    (list "(λx.λy.x) a" '(app (λ x (λ y x)) a))               ; const
    (list "(λf.λx.f x) g" '(app (λ f (λ x (app f x))) g))     ; apply
    (list "(λx.(app x x)) z" '(app (λ x (app x x)) z))        ; self-apply
  ))

(for-each (lambda (ex)
  (let* ([name (car ex)]
         [expr (cadr ex)]
         [result (normalize expr 100)])
    (display (string-append "  " name)) (newline)
    (display (string-append "    => " (pp-lambda (cdr result)))) (newline)
    (display (string-append "    (steps: " (number->string (car result)) ")")) (newline)
    (newline)))
  examples)

;;; ----- Section 6: Church Lists -----
(display-section "SECTION 6: CHURCH-ENCODED LISTS")
(display "  nil = λc.λn.n   cons = λh.λt.λc.λn. c h (t c n)") (newline)
(newline)

(define my-list (list->church-list '(1 2 3 4 5)))
(assert-equal "Church list [1..5]" (church-list->list my-list) '(1 2 3 4 5))
(assert-equal "church-fold sum"
  (church-fold + 0 my-list) 15)
(assert-equal "church-fold product"
  (church-fold * 1 my-list) 120)

;;; ----- Section 7: Combinators -----
(display-section "SECTION 7: CLASSIC COMBINATORS")
(display "  S = λx.λy.λz. x z (y z)") (newline)
(display "  K = λx.λy. x") (newline)
(display "  I = λx. x  (I = S K K)") (newline)
(display "  B = λx.λy.λz. x (y z)  (function composition)") (newline)
(display "  C = λx.λy.λz. x z y    (flip arguments)") (newline)
(newline)

(define S (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
(define K (lambda (x) (lambda (y) x)))
(define I (lambda (x) x))
(define B (lambda (x) (lambda (y) (lambda (z) (x (y z))))))
(define C (lambda (x) (lambda (y) (lambda (z) ((x z) y)))))

(assert-equal "I 42"          ((I) 42) 42)
(assert-equal "K 1 2"         (((K) 1) 2) 1)
(assert-equal "SKK = I"       ((((S) K) K) 42) 42)  ; S K K = I
(assert-equal "B (+1) (*2) 3" ((((B) (lambda (x) (+ x 1))) (lambda (x) (* x 2))) 3) 7)
(assert-equal "C sub 3 10"    ((((C) -) 3) 10) 7)   ; C sub 3 10 = 10-3 = 7

(newline)
(display "============================================================") (newline)
(display "  Lambda Calculus complete!") (newline)
(display "  Church bool/nums/lists * beta reduction * Y * SKI combinator") (newline)
(display "============================================================") (newline)
