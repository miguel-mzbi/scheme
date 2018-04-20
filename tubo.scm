(define pi (* 4 (atan 1)))

(define (area-circle r)
    (* pi r r)
)

(define (base-tubo r g)
    (- (area-circle (+ r g)) (area-circle r))
)

(define (rect-tubo r h)
    (* pi 2 r h)
)

(define (volume-cyl r h)
    (* (area-circle r) h)
)

(define (area-tubo r h g)
    (+ (* 2 (base-tubo r g)) (rect-tubo r h) (rect-tubo (+ r g) h))
)

(define (volume-tubo r h g)
    (- (volume-cyl r h) (volume-cyl (+ r g) h))
)

(define (vol-area-tubo r g h)
    (+ (volume-tubo r g h) (area-tubo r g h))
)

(display "Function:\n(vol-area-tubo r g h)")
(display "\n")