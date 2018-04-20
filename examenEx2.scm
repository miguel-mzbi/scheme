(define (elimina-n n lista)
    (if (or (not (number? n)) (not (> n 0)))
        #f
        (if (or (null? lista) (not (list? lista)))
            #f
            (if (not (checar-length n lista))
                #f
                (elimina-n-postCheck n lista)
            )
        )
    )
)

(define (checar-length n lista)
    (if (null? (cdr lista))
        (if (> n 1)
            #f
            #t
        )
        (checar-length (- n 1) (cdr lista))
    )
)

(define (elimina-n-postCheck n lista)
    (if (= n 1)
        (cdr lista)
        (cons (car lista) (elimina-n-postCheck (- n 1) (cdr lista)))
    )
)

(define (elimina-n-show n lista)
    (display "Lista: ")
    (display lista)
    (display "\n")
    (display "Eliminando: ")
    (display n)
    (display "\n")
    (display "Nueva lista: ")
    (display (elimina-n n lista))
    (display "\n\n")
)

(elimina-n-show 2 '())
(elimina-n-show 4 '(1 2 #f))
(elimina-n-show 3 '(1 2 3 4 5 6))
(elimina-n-show 4 '(a b c d))

(exit)