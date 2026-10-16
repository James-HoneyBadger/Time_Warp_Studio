; Closures and local state in Scheme
(define (make-counter start)
  (lambda ()
    (set! start (+ start 1))
    start))

(define counter (make-counter 0))
(display "counter: ")
(display (counter))
(display " ")
(display (counter))
(display " ")
(display (counter))
(newline)

(define (make-adder n)
  (lambda (x) (+ x n)))

(define add10 (make-adder 10))
(define add100 (make-adder 100))
(display "add10 5   = ") (display (add10 5)) (newline)
(display "add100 5  = ") (display (add100 5)) (newline)

; let-over-lambda style accumulator
(define (make-accumulator total)
  (lambda (amount)
    (set! total (+ total amount))
    total))

(define acc (make-accumulator 100))
(display "accumulate: ")
(display (acc 10)) (display " ")
(display (acc 20)) (display " ")
(display (acc 30))
(newline)
