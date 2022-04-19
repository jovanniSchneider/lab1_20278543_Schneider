#lang racket
(provide (all-defined-out))
(require "card_20278543_SchneiderPizarro.rkt")

;Representacion
;cardsSet = cantidadCartas(number) X cantidadSimbolos(number) X orden(number) X cartas(list) X rndFn

;------------------------------Funcion contructor----------------------------
;Dominio: Elements (list) X numE(int) X maxC(int) X rndFn (fn)
;Recorrido: cardsSet
;Explicacion: Función constructora de conjuntos válidos de cartas para el juego Dobble.

(define cardsSet (lambda (elements numE maxC rndFn)
        (define orderAndMaxCardsPossible (lazy (calcularCantSymbols numE 0)))
        (if (and (list? elements) (and (integer? numE) (> numE 0)) (integer? maxC) (procedure? rndFn))
            (if (>= (length elements) (cdr (force orderAndMaxCardsPossible)))
                (cardsSet->generarConjunto elements numE maxC (car (force orderAndMaxCardsPossible)) null 1)
                null
            )
            (force orderAndMaxCardsPossible)
        )
    )
)

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

;Dominio: cardinal (int) X lista (list)
;Recorrido: Bool
;Funcion: Verifica si todas las cartas de un aspirante a cardsSet tienen el mismo cardinal (cantidad de cartas)

(define cardsSet->mismoCardinal (lambda (cardinal lista)
        (= (length lista) (length (filter (lambda (carta) (= (card->getCardinal carta) cardinal)) lista)))
    )
)

;Dominio: Int (cantidad de simbolos por carta)
;Recorrido: Pair (order MaxCards)
;Descripcion: calcula el orden y la cantidad maxima de cartas posibles a producir (misma cantidad de simbolos distintos necesarios)
;Recursion: De cola
(define calcularCantSymbols (lambda(SymbolsPerCard order)
        (if (and (= order 0) (not(equal? SymbolsPerCard 1)))
            (calcularCantSymbols SymbolsPerCard (- SymbolsPerCard 1))
            (cons order (+ 1 (expt order 2) order)) 
        )
    )
)

;Dominio:
;Recorrido:
;Descripcion:
;Recursion:
(define cardsSet->generarConjunto (lambda (elements numE maxCard order preConjunto fase)
        (if (>= (length preConjunto) maxCard)
            preConjunto
            (if (= fase 1)
                (cardsSet->generarConjunto (append elements (list 1 2 3)) numE maxCard order (append preConjunto (list (generarConjunto->firstCard elements null numE)))2)
                (if (= fase 2)
                    (cardsSet->generarConjunto elements numE maxCard order (append preConjunto (generarConjunto->nCards elements order null 1 1 (list (card->seleccionarNthElement elements 1 0)))) 3)
                    (cardsSet->generarConjunto elements numE maxCard order (append preConjunto (generarConjunto->n2Cards elements order null 1 1 1 (list (card->seleccionarNthElement elements 2 0)))) 3)
                )
            )
        )
    )
)

(define generarConjunto->firstCard (lambda (elementos carta numE)
        (if (= (length carta) numE)
            (card numE carta)
            (generarConjunto->firstCard (cdr elementos) (append carta (list (car elementos))) numE)
        )
    )
)

(define generarConjunto->nCards (lambda (elements order preConjunto j k preCarta) 
        (if (and (> j order) (> k order))
            preConjunto
            (if (= k (+ order 1))
                (generarConjunto->nCards elements order (append preConjunto (list (card (+ order 1) preCarta))) (+ j 1) 1 (list (card->seleccionarNthElement elements 1 0)))
                (generarConjunto->nCards elements order preConjunto j (+ k 1) (append preCarta (list (card->seleccionarNthElement elements (+ (* order j) (+ k 1)) 0))))
            )
        )
    )
)

(define generarConjunto->n2Cards (lambda (elements order preConjunto i j k preCarta)
        (if (and(>= i order)(> j order)(>= k order)) 
            preConjunto
            (if (and (> j order) (> k order))
                (generarConjunto->n2Cards elements order preConjunto (+ 1 i) 1 1 (list (card->seleccionarNthElement elements (+ i 2) 0 )))
                (if (> k order )
                    (generarConjunto->n2Cards elements order (append preConjunto (list (card (+ 1 order) preCarta))) i (+ j 1) 1 (list (card->seleccionarNthElement elements (+ i 1) 0)))
                    (generarConjunto->n2Cards elements order preConjunto i j (+ k 1) (append preCarta (list (card->seleccionarNthElement elements (+ order 2 (* order (- k 1)) (modulo (+ (- j 1)(* (- i 1) (- k 1))) order)) 0))))
                )
            )
        )
    )
)


;Pruebas
;(define conjunto (list 3 3 3 (list (card 3 "pepe" "juan" "s") (card 3 "jaja" "juan" "a") (card 3 "pepe" "caca" "a"))))
;(define a (string-split "0 1 2 3 4 5 6 7 8 9" " "))
(generarConjunto->nCards (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) 3 null 1 1 (list 1))