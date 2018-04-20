(define (elimina-elemento e lista)
    (if (not (or (number? e) (symbol? e)))
        #f
        (if (null? lista)
            #f
            (if (not (checar-lista lista))
                #f
                (elimina-elemento-postCheck e lista)
            )
        )
    )
)

(define (checar-lista lista)
    (if (null? (cdr lista))
        (if (or (number? (car lista)) (symbol? (car lista)))
            #t
            #f
        )
        (if (or (number? (car lista)) (symbol? (car lista)))
            (checar-lista (cdr lista))
            #f
        )
    )
)

(define (bothSymbolsEqual? e l)
    (if (and (symbol? e) (symbol? l))
        (if (equal? e l)
            #t
            #f
        )
        #f
    )
)

(define (bothNumbersEqual? e l)
    (if (and (number? e) (number? l))
        (if (= e l)
            #t
            #f
        )
        #f
    )
)

(define (elimina-elemento-postCheck e lista)
    (if (null? (cdr lista))
        (cond 
            [(bothNumbersEqual? e (car lista)) '()]
            [(bothSymbolsEqual? e (car lista)) '()]
            [else (cons (car lista) '())]
        )
        (cond 
            [(bothNumbersEqual? e (car lista)) (elimina-elemento-postCheck e (cdr lista))]
            [(bothSymbolsEqual? e (car lista)) (elimina-elemento-postCheck e (cdr lista))]
            [else (cons (car lista) (elimina-elemento-postCheck e (cdr lista)))]
        )
    )
)

(define (elimina-elemento-show e lista)
    (display "Lista: ")
    (display lista)
    (display "\n")
    (display "Eliminando: ")
    (display e)
    (display "\n")
    (display "Nueva lista: ")
    (display (elimina-elemento e lista))
    (display "\n\n")
)

(elimina-elemento-show 2 '())
(elimina-elemento-show 2 '(1 2 a m #f))
(elimina-elemento-show 'a '(1 2 3 2 5 6 a 9))
(elimina-elemento-show 2 '(2 2 2 2))

(exit)