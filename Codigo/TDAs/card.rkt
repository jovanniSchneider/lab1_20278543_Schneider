#lang racket

(provide (all-defined-out))
(require "../functions.rkt")

;Representacion: 
;card = Simbolo1 X Simbolo2 X ... X SimboloN

;------------Constructor-----------
;Dom: integer x list
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
        (if (null? carta)
            (append carta (list symbol))
            (if (card? carta)
                (append carta (list symbol))
                null
            )
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

;Dominio: card x card
;Recorrido: Bool
;Funcion: Verifica que 2 cartas solo contengan 1 elemento en comun
(define card->just1Match(lambda(carta1 carta2)
        (card->just1MatchAux carta1 carta2 0)
    )
)

;Dominio: card x card
;Recorrido: Bool
;Funcion: Verifica que 2 cartas solo contengan 1 elemento en comun utilizando recursion de cola
(define card->just1MatchAux(lambda(carta1 carta2 matches)
        (if (> matches 1)
            #f
            (if (= (length carta1) 0)
                (if (= matches 0)
                    #f
                    #t
                )
                (if (card->SymbolIsIn? (card->getFirstSymbol carta1) carta2)
                    (card->just1MatchAux (card->getNextSymbols carta1) carta2 (+ matches 1))
                    (card->just1MatchAux (card->getNextSymbols carta1) carta2 matches)
                )
            )
        )
    )
)

(define card->SymbolIsIn? (lambda (symbol carta)
        (if (not(= (length (filter (lambda (simbolo) (if (char? simbolo) (char=? simbolo symbol) (equal? simbolo symbol))) carta)) 0))
            #t
            #f
        )
    )
)