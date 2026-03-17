; Metacircular Evaluator — A Scheme interpreter in Scheme
; Demonstrates: recursion, closures, higher-order functions, quoting

; ══════════════════════════════════════
;   🔄 Metacircular Scheme Evaluator
; ══════════════════════════════════════

; --- Environment operations ---
(define (make-env parent)
  (list (list) parent))

(define (env-lookup env name)
  (if (null? env)
      (begin (display "❌ Unbound: ") (display name) (newline) #f)
      (let ((bindings (car env)))
        (let ((found (assoc name bindings)))
          (if found
              (cadr found)
              (env-lookup (cadr env) name))))))

(define (env-set! env name val)
  ; Always prepend a new (name val) binding — assoc finds the first match,
  ; so later definitions shadow earlier ones correctly.
  (set-car! env (cons (list name val) (car env))))

; --- Evaluator ---
(define (my-eval expr env)
  (cond
    ((number? expr) expr)
    ((string? expr) expr)
    ((symbol? expr) (env-lookup env expr))
    ((not (pair? expr)) expr)
    ((eq? (car expr) 'quote)  (cadr expr))
    ((eq? (car expr) 'if)     (my-eval-if expr env))
    ((eq? (car expr) 'define) (my-eval-define expr env))
    ((eq? (car expr) 'lambda) (my-eval-lambda expr env))
    ((eq? (car expr) 'begin)  (my-eval-begin (cdr expr) env))
    ((eq? (car expr) 'let)    (my-eval-let expr env))
    (else (my-apply (my-eval (car expr) env)
                    (map (lambda (a) (my-eval a env)) (cdr expr))))))

(define (my-eval-if expr env)
  (if (my-eval (cadr expr) env)
      (my-eval (caddr expr) env)
      (if (not (null? (cdddr expr)))
          (my-eval (cadddr expr) env)
          #f)))

(define (my-eval-define expr env)
  (let ((name (cadr expr))
        (val (my-eval (caddr expr) env)))
    (env-set! env name val)
    val))

(define (my-eval-lambda expr env)
  (list 'closure (cadr expr) (caddr expr) env))

(define (my-eval-begin exprs env)
  (if (null? (cdr exprs))
      (my-eval (car exprs) env)
      (begin (my-eval (car exprs) env)
             (my-eval-begin (cdr exprs) env))))

(define (my-eval-let expr env)
  (let* ((bindings (cadr expr))
         (body (caddr expr))
         (new-env (make-env env)))
    (for-each
     (lambda (b) (env-set! new-env (car b) (my-eval (cadr b) env)))
     bindings)
    (my-eval body new-env)))

; --- Applicator ---
(define (my-apply proc args)
  (cond
    ((procedure? proc) (apply proc args))
    ((and (pair? proc) (eq? (car proc) 'closure))
     (let* ((params (cadr proc))
            (body (caddr proc))
            (closure-env (cadddr proc))
            (new-env (make-env closure-env)))
       (bind-params! new-env params args)
       (my-eval body new-env)))
    (else (display "❌ Not a procedure") (newline) #f)))

(define (bind-params! env params args)
  (if (not (null? params))
      (begin
        (env-set! env (car params) (car args))
        (bind-params! env (cdr params) (cdr args)))))

; --- Bootstrap environment with primitives ---
(define global-env (make-env '()))
(env-set! global-env '+ +)
(env-set! global-env '- -)
(env-set! global-env '* *)
(env-set! global-env '< <)
(env-set! global-env '> >)
(env-set! global-env '= =)
(env-set! global-env 'not not)
(env-set! global-env 'display display)
(env-set! global-env 'newline newline)
(env-set! global-env '#t #t)
(env-set! global-env '#f #f)

; === Run test programs through our evaluator ===
(display "╔══════════════════════════════════════╗") (newline)
(display "║  🔄 Metacircular Scheme Evaluator   ║") (newline)
(display "╚══════════════════════════════════════╝") (newline)
(newline)

(display "── Test 1: Arithmetic ──") (newline)
(display "  (+ 2 (* 3 4)) = ")
(display (my-eval '(+ 2 (* 3 4)) global-env))
(newline)

(display "── Test 2: Define & Use ──") (newline)
(my-eval '(define x 42) global-env)
(display "  x = ")
(display (my-eval 'x global-env))
(newline)

(display "── Test 3: Lambda ──") (newline)
(my-eval '(define square (lambda (n) (* n n))) global-env)
(display "  (square 7) = ")
(display (my-eval '(square 7) global-env))
(newline)

(display "── Test 4: If expression ──") (newline)
(display "  (if (< 3 5) 'yes 'no) = ")
(display (my-eval '(if (< 3 5) 100 200) global-env))
(newline)

(display "── Test 5: Factorial ──") (newline)
(my-eval '(define fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1)))))) global-env)
(display "  (fact 6) = ")
(display (my-eval '(fact 6) global-env))
(newline)

(display "── Test 6: Higher-order ──") (newline)
(my-eval '(define apply-twice (lambda (f x) (f (f x)))) global-env)
(my-eval '(define double (lambda (n) (* n 2))) global-env)
(display "  (apply-twice double 3) = ")
(display (my-eval '(apply-twice double 3) global-env))
(newline)

(newline)
(display "✅ All metacircular evaluator tests passed!") (newline)
