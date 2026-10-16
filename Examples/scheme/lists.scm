; Working with lists in Scheme
(define numbers (list 5 3 9 1 7 2))

(display "list       : ") (display numbers) (newline)
(display "car        : ") (display (car numbers)) (newline)
(display "cdr        : ") (display (cdr numbers)) (newline)
(display "length     : ") (display (length numbers)) (newline)
(display "reverse    : ") (display (reverse numbers)) (newline)
(display "doubled    : ") (display (map (lambda (n) (* n 2)) numbers)) (newline)
(display "odds only  : ") (display (filter odd? numbers)) (newline)
(display "sum        : ")
(display (foldl + 0 numbers))
(newline)

; Build a list recursively
(define (range a b)
  (if (>= a b)
      '()
      (cons a (range (+ a 1) b))))

(display "range 1-5  : ")
(display (range 1 6))
(newline)
