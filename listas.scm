(define lista '(#t E () (1 2 3 4 5) 1 2 3 4 5))


(define (obten-cinco)
    (car (cdr (cdr (cdr (cdr (car (cdr (cdr (cdr lista)))))))))
)

(define (last x)
    (cond
        [(null? x) #f]
        [(null? (cdr x)) (car x)]
        [(pair? x) (last (cdr x))]
    )
)

(define (append listA listB)
    (if (null? listA)
        listB
        (cons 
            (car listA) 
            (append (cdr listA) listB)
        )
    )
    ; (cond 
    ;     [(null? listA) listB]
    ;     [else 
    ;         (cons (car listA) (append (cdr listA) listB))
    ;     ]
    ; )
)

(define (reverse listA)
    (define (reverseAux listA res)
        (cond 
            [(null? listA) res]
            [else
                (reverseAux (cdr listA) (cons (car listA) res))
            ]
        )
    )
    (reverseAux listA (list))
)

(define (sumList listA)
    (define (sumAux listA res)
        (cond 
            [(null? listA) res]
            [else
                (sumAux (cdr listA) (+ res (car listA)))
            ]
        )
    )
    (sumAux listA 0)
)

(define (countList listA)
    (define (countAux listA res)
        (cond 
            [(null? listA) res]
            [else
                (countAux (cdr listA) (+ res 1))
            ]
        )
    )
    (countAux listA 0)
)

(define (enumerate n)
    (define (enumerateAux x list)
        (if (= x 0)
            (cons x list)
            (enumerateAux (- x 1) (cons x list))
        )
    )
    (enumerateAux n '())
)

(define (pointProduct a b)
    (define (pointAux a b total)
        (if (null? a)
            total
            (pointAux (cdr a) (cdr b) (+ total (* (car a) (car b))))
        )
    )
    (pointAux a b 0)
)


(define listA (list 2 4 6))
(define listB (list 8 10 12 14))

(display (enumerate 10))
(display "\n")

(display (pointProduct (list 1 2 3) (list 1 2 3)))
(display "\n")

(display (append listA listB))
(display "\n")

(display (reverse listA))
(display "\n")

(display (sumList (append listA listB)))
(display "\n")

(display (countList (append listA listB)))
(display "\n")

(display (last (append listA listB)))
(display "\n")

(display "\n")
(exit)