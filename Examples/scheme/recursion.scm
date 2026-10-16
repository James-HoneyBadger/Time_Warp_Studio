; Recursion in Scheme: factorial, Fibonacci and a countdown
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (countdown n)
  (when (> n 0)
    (display n)
    (display " ")
    (countdown (- n 1))))

(display "5! = ")
(display (fact 5))
(newline)

(display "Fibonacci 0..10: ")
(let loop ((i 0))
  (when (<= i 10)
    (display (fib i))
    (display " ")
    (loop (+ i 1))))
(newline)

(display "Countdown: ")
(countdown 5)
(display "liftoff!")
(newline)
