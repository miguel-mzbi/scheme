(define (rev costo)
    (define reductions (/ (- 5.0 costo) 0.10))
    (define attendance (+ 120 (* 15 reductions)))
    (+ (- 180) (* (- 0.04) attendance) (* costo attendance))
)

(display "Costo a 2.9\n")
(display (rev 2.9))
(display "\n")
(exit)