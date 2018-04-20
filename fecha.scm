(define (is-month-31 m)
    (if (or (= m 1) (= m 3) (= m 5) (= m 7) (= m 8) (= m 10) (= m 12))
        #t
        #f
    )
)

(define (is-month-30 m)
    (if (or (= m 4) (= m 6) (= m 9) (= m 11))
        #t
        #f
    )
)

(define (is-leap a)
    (cond 
        [(and (= (mod a 4) 0) (not (= (mod a 100) 0))) #t]
        [(and (= (mod a 4) 0) (= (mod a 100) 0) (= (mod a 400) 0)) #t]
        [else #f]
    )
)

(define (is-february-leap m a)
    (if (= m 2)
        (if (is-leap a)
            #t
            #f
        )
        #f
    )
)

(define (is-month m)
    (if (number? m)
        (if (and (>= m 1) (<= m 12))
            #t
            #f
        )
        #f
    )
)

(define (is-year a)
    (if (number? a)
        (if (and (>= a 1) (<= a 9999))
            #t
            #f
        )
        #f
    )
)

(define (valid-date d m a)
    (if (and (number? d) (>= d 1) (is-month m) (is-year a))
        (cond
            [(and (is-february-leap m a) (<= d 29)) #t]
            [(and (not (is-february-leap m a)) (<= d 28)) #t]
            [(and (is-month-30 m) (<= d 30)) #t]
            [(and (is-month-31 m) (<= d 31)) #t]
            [else #f]
        )
        #f
    )
)

(display "Function:\n(valid-date dd mm yyyy)")
(display "\n")