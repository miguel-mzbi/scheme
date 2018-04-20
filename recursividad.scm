(define (factorial n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))
    )
)
(display (factorial 3))
(display "\n")


(define (factorialPub n)
    (define (factorialPriv n r)
        (if (= n 1)
            r
            (factorialPriv (- n 1) (* n r))
        )
    )
    (factorialPriv n 1)
)

(display (factorialPub 4))
(display "\n")

(define (fibonacciIf n)
    (if (= n 0)
        0
        (if (= n 1)
            1
            (+ (fibonacciIf (- n 1)) (fibonacciIf (- n 2)))
        )
    )
)

(display (fibonacciIf 7))
(display "\n")

(define (fibonacciIt n)
    (define (fibonacci-help a b n)
        (if (= n 0)
            b
            (fibonacci-help (+ a b) a (- n 1))
        )
    )
    (fibonacci-help 1 0 n)
)


(display (fibonacciIt 2))
(display "\n")

(exit)