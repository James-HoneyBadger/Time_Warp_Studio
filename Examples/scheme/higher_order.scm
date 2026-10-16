; Higher-order procedures: map, filter, foldl and compose
(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(display "squares of 1..6 : ")
(display (map square (list 1 2 3 4 5 6)))
(newline)

(display "evens of 1..10  : ")
(display (filter even? (list 1 2 3 4 5 6 7 8 9 10)))
(newline)

(display "product 1..5    : ")
(display (foldl * 1 (list 1 2 3 4 5)))
(newline)

(define inc-then-square (compose square inc))
(display "inc-then-square 9: ")
(display (inc-then-square 9))
(newline)

(display "applied        : ")
(display (apply + (list 10 20 30)))
(newline)
