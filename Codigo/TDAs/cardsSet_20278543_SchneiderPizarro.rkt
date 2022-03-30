#lang racket
(provide (all-defined-out))
(require "card_20278543_SchneiderPizarro.rkt")

;Representacion
;cardsSet = cantidadCartas(number) X cantidadSimbolos(number) X orden(number) X cartas(list)

;------------------------------Funcion contructor----------------------------

;----------------------------Funcion de Pertenencia--------------------------
(define cardsSet? (lambda (lista)
        (if (and (number? (cardsSet->getCantCartas lista)) (number? (cardsSet->getCantSimbolos lista)) (number? (cardsSet->getOrden lista) ) (list? (cardsSet->getCartas lista)))
           (if (= (cardsSet->getCantCartas lista) (cardsSet->getCantSimbolos lista))
                (if (cardsSet->mismoCardinal (cardsSet->getCantSimbolos lista) (cardsSet->getCartas lista))
                    #t 
                    #f
                )
                #f
           )
           #f 
        )
    )
)

;---------------------------------Selectores----------------------------------
(define cardsSet->getCantCartas car)
(define cardsSet->getCantSimbolos cadr)
(define cardsSet->getOrden caddr)
(define cardsSet->getCartas cadddr)


;----------------------------Funciones propias-----------------------------
(define cardsSet->mismoCardinal (lambda (cardinal lista)
        (= (length lista) (length (filter (lambda (carta) (= (card->getCardinal carta) cardinal)) lista)))
    )
)



;Pruebas
(define conjunto (list 3 3 3 (list (card 3 "pepe" "juan" "s") (card 3 "jaja" "juan" "a") (card 3 "pepe" "caca" "a"))))
(define a (string-split "0 1 2 3 4 5 6 7 8 9" " "))
