;;; Hello World in Scheme
;;; Demonstrates: display, newline, basic arithmetic, string operations

;; Classic greeting
(display "Hello, World!") (newline)

;; Basic arithmetic
(display "--- Basic Arithmetic ---") (newline)
(display "2 + 3 = ")    (display (+ 2 3))    (newline)
(display "10 - 4 = ")   (display (- 10 4))   (newline)
(display "6 * 7 = ")    (display (* 6 7))    (newline)
(display "22 / 7 = ")   (display (/ 22 7))   (newline)
(display "2^10 = ")     (display (expt 2 10)) (newline)

;; Comparison and boolean logic
(display "--- Logic ---") (newline)
(display "(> 5 3) = ")  (display (> 5 3))  (newline)
(display "(= 4 4) = ")  (display (= 4 4))  (newline)
(display "(and #t #f) = ") (display (and #t #f)) (newline)
(display "(or #t #f) = ") (display (or #t #f)) (newline)

;; String operations
(display "--- Strings ---") (newline)
(define greeting (string-append "Hello" ", " "Scheme" "!"))
(display greeting) (newline)
(display "Length: ") (display (string-length greeting)) (newline)
(display "Upper:  ") (display (string-upcase greeting)) (newline)
(display "Sub:    ") (display (substring greeting 0 5)) (newline)

;; Variables and let
(display "--- Variables ---") (newline)
(let ((name "Time Warp Studio")
      (version 2))
  (display (string-append "Welcome to " name " v" (number->string version)))
  (newline))

(display "Scheme is fun!") (newline)
