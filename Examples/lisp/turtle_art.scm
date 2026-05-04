;;; Turtle Art in Scheme
;;; Demonstrates: turtle graphics, recursion, fractals

;; --- Helpers ---
(define (repeat n thunk)
  (when (> n 0)
    (thunk)
    (repeat (- n 1) thunk)))

;; --- Square ---
(define (draw-square size)
  (repeat 4
    (lambda ()
      (forward size)
      (right 90))))

;; --- Spiral of squares ---
(define (square-spiral size steps)
  (when (> steps 0)
    (draw-square size)
    (right 15)
    (square-spiral (+ size 4) (- steps 1))))

;; Draw a colourful spiral
(color 50 180 255)
(pendown)
(square-spiral 10 20)

;; --- Snowflake branch ---
(define (branch len depth)
  (when (> depth 0)
    (forward len)
    (left 30)  (branch (* len 0.65) (- depth 1))
    (right 60) (branch (* len 0.65) (- depth 1))
    (left 30)
    (backward len)))

;; Move to left side for tree
(penup)
(home)
(setpos -60 -80)
(setheading 90)
(pendown)
(color 80 220 80)
(branch 40 4)

;; --- Polygon row ---
(define (polygon sides size)
  (let ((angle (/ 360 sides)))
    (repeat sides
      (lambda ()
        (forward size)
        (right angle)))))

;; Row of polygons
(penup)
(setpos -100 60)
(pendown)
(color 255 140 0)
(let loop ((n 3) (x -100))
  (when (<= n 7)
    (penup)
    (setpos x 60)
    (pendown)
    (polygon n 18)
    (loop (+ n 1) (+ x 40))))

;; --- Concentric circles ---
(penup)
(setpos 60 -20)
(pendown)
(let loop ((r 5) (step 0))
  (when (< step 8)
    (color (+ 100 (* step 18)) (- 200 (* step 15)) 220)
    (circle r)
    (penup)
    (setpos 60 (- -20 r))
    (pendown)
    (loop (+ r 7) (+ step 1))))

(display "Turtle art complete!") (newline)
