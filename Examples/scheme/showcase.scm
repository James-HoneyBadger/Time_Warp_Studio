; ============================================================
; THE EXPRESSION MACHINE
; A Scheme showcase: numbers and strings, recursion, lists,
; higher-order procedures, closures, and turtle graphics.
; ============================================================

(define (say x) (display x) (newline))

(say "T H E   E X P R E S S I O N   M A C H I N E")
(say "----------------------------------------")

; --- 1. Numbers ---------------------------------------------
(say (+ 1 2 3 4))
(say (* 6 7))
(say (/ 7 2))
(say (expt 2 10))
(say (sqrt 144))
(say (max 3 9 2))
(say (min 3 9 2))

; --- 2. Strings and symbols ---------------------------------
(say (string-append "Time " "Warp"))
(say (string-length "turtle"))
(say (substring "turtle" 1 4))
(say (string-upcase "shout"))
(say (number->string 42))
(say (list 'alpha 'beta 'gamma))

; --- 3. Definitions and recursion ---------------------------
(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
(say (fact 10))

(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(say (fib 12))

(define (countdown n)
  (when (> n 0)
    (display n)
    (display " ")
    (countdown (- n 1))))
(countdown 5)
(newline)

; --- 4. Lists and higher-order procedures -------------------
(define (square x) (* x x))
(say (map square (list 1 2 3 4 5)))
(say (filter odd? (list 1 2 3 4 5 6 7)))
(say (foldl + 0 (list 1 2 3 4 5)))
(say (apply + (list 10 20 30)))
(say (reverse (list 1 2 3)))
(say (append (list 1 2) (list 3 4)))
(say (length (list 'a 'b 'c)))
(say (list-ref (list 'a 'b 'c) 1))

; --- 5. Local bindings and closures -------------------------
(say (let ((x 3) (y 4)) (+ (* x x) (* y y))))
(say (let* ((x 3) (y (* x 2))) (+ x y)))

(define (make-counter)
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      n)))
(define tick (make-counter))
(say (tick))
(say (tick))
(say (tick))

; --- 6. Conditionals ----------------------------------------
(define (classify n)
  (cond ((< n 0) "negative")
        ((= n 0) "zero")
        (else "positive")))
(say (classify -4))
(say (classify 0))
(say (classify 9))
(when (> 5 2) (say "when: five is greater than two"))
(unless (> 2 5) (say "unless: two is not greater than five"))

; --- 7. A counted loop as an expression ---------------------
(say (do ((i 0 (+ i 1))
          (sum 0 (+ sum i)))
         ((= i 5) sum)))

; --- 8. Turtle graphics -------------------------------------
(say "Drawing polygons, a tree and a circle.")

(define (polygon sides length)
  (define (loop n)
    (when (> n 0)
      (forward length)
      (right (/ 360 sides))
      (loop (- n 1))))
  (loop sides))

(color 255 255 0)
(polygon 6 45)
(penup)
(forward 90)
(pendown)
(color 0 200 255)
(polygon 3 40)
(penup)
(forward 80)
(pendown)

(define (tree size)
  (when (> size 6)
    (forward size)
    (right 30)
    (tree (/ size 2))
    (left 60)
    (tree (/ size 2))
    (right 30)
    (back size)))

(color 0 220 80)
(home)
(tree 50)
(penup)
(home)
(pendown)
(color 255 120 0)
(circle 30)
(home)
(say "Drawing complete.")
