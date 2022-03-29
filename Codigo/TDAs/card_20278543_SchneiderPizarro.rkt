#lang racket
(provide (all-defined-out))

;Representacion:
;card = cantidadSimbolos(number) x simbolos(list)

;-------------------------Funcion constructor--------------------------
;Dominio: number X list
;Recorrido: list
;Funcion: Crea una entidad de card en caso de que los parametros ingresados correspondan al dominio del TDA
(define card (lambda (cardinal . figuras)
    (define preCard (lazy (list cardinal figuras)))
        (if (card? (force preCard))
            (force preCard)
            null
        )        
    )
)

;-------------------------Funcion pertenencia--------------------------
;Dominio: list
;Recorrido: Bool
;Funcion: Verifica si una lista corresponde al TDA card, devuelve true o false
(define card? (lambda (lista)
        (if (and (number? (car lista)) (list? (cdr lista)))
            #t
            #f
        )
    )
)

;------------------------------Selectores------------------------------
;Entrega el numero de simbolos que contiene la carta
(define card->getCardinal car)
;Entrega la lista de simbolos de la carta
(define card->getSimbolos cadr)


(define e1 (card 3 "pepe" "juan" "s"))
