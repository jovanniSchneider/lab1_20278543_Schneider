#lang racket
(provide (all-defined-out))

;Representacion
;cardsSet = card1 X card2 X ... X cardN

;----------Constructor------------
;Dom: Elements (list) X numE(int) X maxC(int) X rndFn (fn)
;Rec: cardsSet

(define cardsSet (lambda(elements numE maxC rndFn)
        (if (and(list? elements)(integer? numE)(integer? maxC)(procedure? rndFn))
            #t
            null
        )
    )
)


;------------Funciones propias------------
(define order (lambda (numE)
        (- numE 1)
    )
)

