#lang racket
(provide (all-defined-out))

;Representacion:
;card = cantidadSimbolos(number) x simbolos(list)

;-------------------------Funcion constructor--------------------------
;Dominio: number X list
;Recorrido: list
;Funcion: Crea una entidad de card en caso de que los parametros ingresados correspondan al dominio del TDA
(define card (lambda (cardinal figuras)
        (if (and (number? cardinal) (list? figuras) (= cardinal (length figuras)))
            (list cardinal figuras)
            null
        )        
    )
)

;-------------------------Funcion pertenencia--------------------------
;Dominio: list
;Recorrido: Bool
;Funcion: Verifica si una lista corresponde al TDA card, devuelve true o false
(define card? (lambda (lista)
        (if (and (list? lista) (not (null? lista)))
            (if (and (number? (car lista)) (list? (cdr lista)) (= 2 (length lista)))
                (= (car lista) (length (cadr lista)))
                #f
            )
            #f
        )
    )
)

;------------------------------Selectores------------------------------
;Entrega el numero de simbolos que contiene la carta
(define card->getCardinal (lambda (lista)
        (if (card? lista)
            (car lista)
            null
        )
    )
)
;Entrega la lista de simbolos de la carta
(define card->getSimbolos (lambda (lista)
        (if (card? lista)
            (cadr lista)
            null
        )
    )
)

;--------------------------Funciones propias---------------------------

;Dominio: card
;Recorrido: Bool
;Funcion: Verifica si una carta solo tiene simbolos distintos

(define card->JustDifferentSymbols (lambda (carta)
        (if (list? (card->getSimbolos carta))
            (= (length (card->getSimbolos carta)) (length (filter (card->just1Time (card->getSimbolos carta)) (card->getSimbolos carta))))
            #f
        )
    )
)

;Dominio: card
;Recorrido: Bool
;Funcion: Verifica que un simbolo solo aparezca una vez en una lista
(define card->just1Time (lambda (listaDeSimbolos) 
            (lambda (simbolo)
                (= 1 (length (filter (lambda (simb)(equal? simb simbolo))listaDeSimbolos))) 
        )
    )
)

(define card->seleccionarNthElement (lambda (elements posicion counter)
        (if (= (+ 1 counter) posicion)
            (car elements)
            (card->seleccionarNthElement (cdr elements) posicion (+ counter 1))
        )
    )
)

;Ejemplo de uso
(define e1 (card 3 (list 1 2 2)))
