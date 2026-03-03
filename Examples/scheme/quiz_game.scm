;;; ================================================================
;;; quiz_game.scm — Interactive Quiz Using Association Lists
;;; Demonstrates: assoc, association lists, recursion, closures,
;;; let/letrec, cond, higher-order functions, and I/O in Scheme.
;;; ================================================================

;;; ──────────────────────────────────────────────────────────────
;;;  QUIZ DATA (association list of question records)
;;; ──────────────────────────────────────────────────────────────
;;;  Each record: (question choices correct-idx explanation)

(define *quiz-data*
  (list
    '("What is the result of (car '(1 2 3)) in Scheme?"
      ("1" "2" "3" "'(1 2 3)")
      0
      "car returns the first element of a pair.")

    '("Which of these creates a new pair in Scheme?"
      ("list" "cons" "car" "cdr")
      1
      "cons constructs a pair from two values.")

    '("What does (cdr '(a b c)) return?"
      ("a" "(a b c)" "(b c)" "b")
      2
      "cdr returns the tail of a list, which is (b c).")

    '("Which is a tail-recursive form of factorial?"
      ("(define (f n) (if (= n 0) 1 (* n (f (- n 1)))))"
       "(define (f n acc) (if (= n 0) acc (f (- n 1) (* n acc))))"
       "(define (f n) (apply * (iota n 1)))"
       "Both A and B")
      1
      "Version B uses an accumulator, making it tail-recursive.")

    '("What is a 'closure' in Scheme?"
      ("A type of loop construct"
       "A function with its enclosing variable environment"
       "A way to terminate a program"
       "A special form for error handling")
      1
      "A closure captures the lexical environment where it was created.")

    '("What does (map car '((1 2)(3 4)(5 6))) return?"
      ("(1 3 5)" "(2 4 6)" "((1)(3)(5))" "Error")
      0
      "map applies car to each pair, extracting first elements.")

    '("Which defines a local variable in Scheme?"
      ("local" "def" "let" "var")
      2
      "let creates local bindings in Scheme.")

    '("What is the value of (+ 1/2 1/3)?"
      ("0.833" "5/6" "2/5" "1/6")
      1
      "Scheme supports exact rational arithmetic: 1/2 + 1/3 = 5/6.")

    '("What does (apply + '(1 2 3 4)) return?"
      ("10" "(10)" "Error" "1 2 3 4")
      0
      "apply calls + with the list elements as arguments: (+ 1 2 3 4) = 10.")

    '("Which is NOT a Scheme boolean?"
      ("#t" "#f" "true" "(not #f)")
      2
      "Scheme uses #t/#f. 'true' as a symbol is truthy but not the boolean literal.")

    '("What does (define (curry f) (lambda (x) (lambda (y) (f x y)))) do?"
      ("Creates a recursive function"
       "Returns a function that takes arguments one at a time"
       "Defines a pattern match"
       "Creates a mutable variable")
      1
      "This is currying: transforms f(x,y) into f(x)(y).")

    '("What is (fold-right cons '() '(1 2 3))?"
      ("(3 2 1)" "(1 2 3)" "Error" "'()")
      1
      "fold-right with cons and '() reconstructs the list: (1 2 3).")

    '("Which evaluates all sub-expressions and returns the last?"
      ("and" "or" "begin" "cond")
      2
      "begin evaluates each expression in sequence and returns the last value.")

    '("What is tail position in (if test (+ 1 2) (+ 3 4))?"
      ("test" "(+ 1 2) only" "(+ 3 4) only" "Both (+ 1 2) and (+ 3 4)")
      3
      "Both branches of if are in tail position when if itself is in tail position.")

    '("What does the Y combinator allow?"
      ("Memoization"
       "Defining anonymous recursive functions"
       "Lazy evaluation"
       "Mutual recursion only")
      1
      "The Y combinator enables recursion without naming functions explicitly.")
  ))

;;; ──────────────────────────────────────────────────────────────
;;;  ASSOCIATION LIST HELPERS
;;; ──────────────────────────────────────────────────────────────

(define (question rec)  (car rec))
(define (choices  rec)  (cadr rec))
(define (correct  rec)  (caddr rec))
(define (explain  rec)  (cadddr rec))

;;; Look up a record by question text
(define (find-question text db)
  (cond
    ((null? db) #f)
    ((string=? text (question (car db))) (car db))
    (else (find-question text (cdr db)))))

;;; ──────────────────────────────────────────────────────────────
;;;  I/O HELPERS
;;; ──────────────────────────────────────────────────────────────

(define (display-line . args)
  (for-each display args)
  (newline))

(define (repeat-char ch n)
  (let loop ((i 0))
    (unless (= i n) (display ch) (loop (+ i 1))))
  (newline))

(define (display-banner text)
  (let ((len (+ (string-length text) 4)))
    (repeat-char "═" len)
    (display-line "  " text)
    (repeat-char "═" len)))

(define (prompt-int msg lo hi)
  "Read an integer in [lo, hi]. Retry on invalid input."
  (let loop ()
    (display msg)
    (let ((input (read-line)))
      (let ((n (string->number input)))
        (if (and n (integer? n) (>= n lo) (<= n hi))
          n
          (begin
            (display-line "  ✗ Invalid — enter a number between "
                          lo " and " hi)
            (loop)))))))

;;; ──────────────────────────────────────────────────────────────
;;;  QUIZ ENGINE
;;; ──────────────────────────────────────────────────────────────

(define (shuffle-list lst)
  "Fisher-Yates shuffle using a simple pseudo-random spread."
  (let* ((vec (list->vector lst))
         (n   (vector-length vec)))
    (let loop ((i (- n 1)))
      (when (> i 0)
        (let* ((j    (modulo (* (+ i 31) 1103515245 12345) (+ i 1)))
               (tmp  (vector-ref vec i)))
          (vector-set! vec i (vector-ref vec j))
          (vector-set! vec j tmp)
          (loop (- i 1)))))
    (vector->list vec)))

(define (ask-question rec q-num total)
  "Display one question and collect/check the answer.
   Returns 1 if correct, 0 otherwise."
  (newline)
  (display-line "Question " q-num " / " total ":  "
                (question rec))
  (display-line (make-string 60 #\─))

  (let ((ch (choices rec)))
    ;; Print lettered choices
    (let loop ((opts ch) (letter #\A))
      (unless (null? opts)
        (display-line "  " letter ". " (car opts))
        (loop (cdr opts) (integer->char (+ (char->integer letter) 1)))))

    (let* ((answer (prompt-int "\nYour choice (1-4): " 1 (length ch)))
           (idx    (- answer 1))
           (right  (correct rec)))
      (newline)
      (if (= idx right)
        (begin
          (display-line "  ✅  Correct!")
          (display-line "  " (explain rec))
          1)
        (begin
          (display-line "  ❌  Incorrect. Correct answer: "
                        (integer->char (+ (char->integer #\A) right))
                        ". " (list-ref ch right))
          (display-line "  " (explain rec))
          0)))))

(define (run-quiz db num-questions)
  "Run a quiz with num-questions drawn from db."
  (display-banner "SCHEME PROGRAMMING QUIZ")
  (display-line "Answer each question (1-4). Let's see how well you know Scheme!")
  (newline)

  (let* ((questions (shuffle-list db))
         (selected  (let loop ((qs questions) (n num-questions) (acc '()))
                      (cond
                        ((or (null? qs) (= n 0)) (reverse acc))
                        (else (loop (cdr qs) (- n 1) (cons (car qs) acc))))))
         (total     (length selected)))

    (let loop ((remaining selected) (q-num 1) (score 0))
      (if (null? remaining)
        ;; All done — print results
        (begin
          (newline)
          (repeat-char "═" 60)
          (display-line "QUIZ COMPLETE!")
          (repeat-char "─" 60)
          (display-line "Your score: " score " / " total
                        " (" (round (* 100.0 (/ score (max 1 total)))) "%)")
          (let ((pct (/ score (max 1 total))))
            (cond
              ((>= pct 0.9)  (display-line "Outstanding! You really know Scheme!"))
              ((>= pct 0.7)  (display-line "Great job! Strong Scheme knowledge."))
              ((>= pct 0.5)  (display-line "Not bad — keep practicing!"))
              (else          (display-line "Keep studying — you'll get there!"))))
          (repeat-char "═" 60)
          score)

        ;; Ask next question
        (let ((points (ask-question (car remaining) q-num total)))
          (loop (cdr remaining) (+ q-num 1) (+ score points)))))))

;;; ──────────────────────────────────────────────────────────────
;;;  STUDY MODE — show all Q&A without scoring
;;; ──────────────────────────────────────────────────────────────

(define (study-mode db)
  (display-banner "STUDY MODE — Review All Questions")
  (let loop ((qs db) (n 1))
    (unless (null? qs)
      (let ((rec (car qs)))
        (display-line "\n[" n "] " (question rec))
        (let inner ((ch (choices rec)) (letter #\A) (idx 0))
          (unless (null? ch)
            (let ((mark (if (= idx (correct rec)) "✓" " ")))
              (display-line "  " mark " " letter ". " (car ch)))
            (inner (cdr ch)
                   (integer->char (+ (char->integer letter) 1))
                   (+ idx 1))))
        (display-line "  → " (explain rec))
        (loop (cdr qs) (+ n 1))))))

;;; ──────────────────────────────────────────────────────────────
;;;  MAIN MENU
;;; ──────────────────────────────────────────────────────────────

(define (main)
  (display-banner "SCHEME QUIZ GAME")
  (display-line "A " (length *quiz-data*) "-question interactive Scheme quiz")
  (newline)
  (display-line "1. Quick Quiz  (5 questions)")
  (display-line "2. Full Quiz   (" (length *quiz-data*) " questions)")
  (display-line "3. Study Mode  (see all answers)")
  (display-line "4. Exit")
  (newline)
  (let ((choice (prompt-int "Enter choice: " 1 4)))
    (cond
      ((= choice 1) (run-quiz *quiz-data* 5))
      ((= choice 2) (run-quiz *quiz-data* (length *quiz-data*)))
      ((= choice 3) (study-mode *quiz-data*))
      ((= choice 4) (display-line "Goodbye!"))))
  (newline)
  (display-line "Thanks for playing the Scheme Quiz!")
)

;; Entry point
(main)
