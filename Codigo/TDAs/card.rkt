#lang racket

(provide (all-defined-out))

;Representacion: 
;card = Simbolo1 X Simbolo2 X ... X SimboloN

;------------Constructor-----------
;Dom: integet x list
;Recorrido: list|null
(define card (lambda ( cantidad . figuras )
        (if (and (list? figuras) (= (length figuras) cantidad))
            figuras
            null
        )
    )
)

;-------------Pertenencia-------------
;Dom: card
;Recorrido: Boolean
(define card? (lambda(carta)
        (and (list? carta) (not(null? carta)))
    )
)

;-------------Selectores-------------
;Dom: card
;Recorrido: Depende de que tipo de datos esté compuesta la carta
;Entrega el primer elemento de la carta
(define card->getFirstSymbol(lambda (carta)
        (if (card? carta)
            (car carta)
            null
        )
    )
)

;Dom: card
;Recorrido: List
;Entrega el resto de los simbolos sin el primero
(define card->getNextSymbols(lambda (carta)
        (if (card? carta)
            (cdr carta)
            null
        )
    ) 
)

;-------------Modificadores--------------
(define card->addSymbol (lambda(carta symbol)
        (if (card? carta)
            (append carta (list symbol))
            null
        )
    )
)


;------------Funciones propias del TDA------------
;Dom: card
;Recorrido: Bool
;Funcion: Verifica si una carta solo tiene simbolos distintos

(define card->JustDifferentSymbols (lambda (carta)
        (if (card? carta)
            (= (length carta) (length (filter (card->just1Time carta) carta)))
            #f
        )
    )
)

;Dominio: card
;Recorrido: Bool
;Funcion: Verifica que un simbolo solo aparezca una vez en una lista
(define card->just1Time (lambda (carta) 
            (lambda (simbolo)
                (= 1 (length (filter (lambda (simb)(equal? simb simbolo))carta))) 
        )
    )
)