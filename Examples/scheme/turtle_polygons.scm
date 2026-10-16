; Turtle graphics from Scheme
(define (repeat n proc)
  (when (> n 0)
    (proc)
    (repeat (- n 1) proc)))

(define (polygon sides length)
  (repeat sides
          (lambda ()
            (forward length)
            (right (/ 360 sides)))))

(display "Drawing a hexagon and a ring of triangles...")
(newline)

(polygon 6 80)

(penup)
(forward 140)
(pendown)

(repeat 12
        (lambda ()
          (polygon 3 30)
          (right 30)))

(home)
(display "Done!")
(newline)
