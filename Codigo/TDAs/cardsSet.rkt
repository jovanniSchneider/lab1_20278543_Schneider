#lang racket
(provide (all-defined-out))
(require "card.rkt")
(require "../functions.rkt")

;Representacion
;cardsSet = CantidadCartas X Cartas X Elements X rndFn

;----------Constructor------------
;Dom: Elements (list) X numE(int) X maxC(int) X rndFn (fn)
;Rec: cardsSet

(define cardsSet (lambda(elements numE maxC rndFn)
        (define n (lazy (cardsSet->order numE)))
        (define CS (lazy (cardsSet->generateN2Cards (force n) (cardsSet->generateNCards (force n) (cardsSet->generateFirstCard (force n) null elements) elements) elements)))
        (define CSmaxC (lazy (cardsSet->getNCards (force CS) maxC null)))
        (if (and(list? elements)(integer? numE)(integer? maxC)(procedure? rndFn))
            (if (<= maxC 0)
                (list (length(force CS))(force CS) elements rndFn)
                (list (length (force CSmaxC)) (force CSmaxC) elements rndFn)
            )
             null
        )
    )
)

;---------Pertenencia----------

(define cardsSet->dobble? (lambda (CS)
        (if (and (cardsSet->allCardsJustDifferentSymbols (cadr CS)) (cardsSet->allCardsJust1Match (cadr CS)))
            (isPowerOfPrime(cardsSet->order (length(car(cadr CS)))))
            #f
        )
    )
)


;-------Getters-------
;Dom:CS
;Rec: Int|Null
;Devuelve la cantidad de cartas en el set 
(define cardsSet->getCantCartas(λ (CS)
        (if(cardsSet->dobble? CS)
            (car CS)
            null
        )
    )
)

;Dom:CS
;Rec: list(card)
;Devuelve la lista de cartas del set
(define cardsSet->getCartas cadr)
;Dom:CS
;Rec: list(elements)
;Devuelve la lista de elementos con los que se construyó el set
(define cardsSet->getElements (λ (CS)
        (if(cardsSet->dobble? CS)
            (caddr CS)
            null
        )
    )
)

;Dom:CS
;Rec: procedure
;Devuelve la rndFn del set
(define cardsSet->getRndFn (λ (CS) 
        (if(cardsSet->dobble? CS)
            (caddr (cdr CS))    
            null
        )
    )
)

;Dom:CS
;Rec: card|null
;Devuelve la primera carta del set
(define cardsSet->getFirstCard(lambda (CS)
        (if (cardsSet->dobble? CS)
            (car (cardsSet->getCartas CS))    
            null
        )
    )
)

;Dom:CS
;Rec: list(card)
;Devuelve la lista de cartas del set sin contar la primera
(define cardsSet->getNextCards (lambda (CS)  
        (cdr (cardsSet->getCartas CS))    
    )
)

;------------Funciones propias------------

;Dom: int (numero de elementos de una carta del conjunto)
;Rec: int (orden n)
;Funcion que retorna el orden del plano proyectivo del conjunto
(define cardsSet->order (lambda (numE)
        (- numE 1)
    )
)
;Dom: int x list x list
;Rec: list(card)
;Genera la primera carta del conjunto y la agrega al CS
(define cardsSet->generateFirstCard (lambda (n preCS elements)
        (append preCS (list (cardsSet->generateFirstCardAux 1 n null elements)))
    )
)

;Dom:int x int x list x list
;Rec: card
;Crea la primera carta utilizando recursion de cola
(define cardsSet->generateFirstCardAux(lambda (counter n preCard elements)
        (if (= (length preCard) (+ n 1))
            preCard
            (cardsSet->generateFirstCardAux (+ 1 counter) n (card->addSymbol preCard (cardsSet->getIElement elements counter)) elements)
            
        )
    )
)

;Dom:int x list x list
;Rec:list
;Crea las primeras N cartas del conjunto y las agrega al set
(define cardsSet->generateNCards (lambda(n preCS elements)
        (append preCS (cardsSet->generateNCardsAux 1 1 n (card 1 (cardsSet->getIElement elements 1)) null elements))
    )
)

;Dom:int x int x int x list x list x list
;Rec: list(card)
;Genera las N cartas utilizando recursion de cola
(define cardsSet->generateNCardsAux (lambda (counter counter2 n preCard preCS elements)
        (if (= counter (+ n 1))
            preCS
            (if (= counter2 (+ n 1))
                (cardsSet->generateNCardsAux (+ counter 1) 1 n (card 1 (cardsSet->getIElement elements 1)) (append preCS (list preCard)) elements)
                (cardsSet->generateNCardsAux counter (+ counter2 1) n (card->addSymbol preCard (cardsSet->getIElement elements (+ (* n counter) (+ counter2 1)))) preCS elements)
            )
        )
    )
)
;Dom:int x list x list
;Rec:list
;Crea las N^2 cartas del conjunto y las agrega al set
(define cardsSet->generateN2Cards (lambda (n preCS elements)
        (append preCS (cardsSet->generateN2CardsAux 1 1 1 n (card 1 (cardsSet->getIElement elements 2)) null elements))
    )
)
;Dom:int x int x int x int x list x list x list
;Rec: list(card)
;Genera las N^2 cartas utilizando recursion de cola
(define cardsSet->generateN2CardsAux(lambda(c1 c2 c3 n preCard preCS elements)
        (if (= c1 (+ n 1))
            preCS
            (if (= c2 (+ n 1))
                (cardsSet->generateN2CardsAux (+ c1 1) 1 1 n (card 1 (cardsSet->getIElement elements (+ c1 1))) preCS elements)
                (if (= c3 (+ n 1))
                    (cardsSet->generateN2CardsAux c1 (+ c2 1) 1 n (card 1 (cardsSet->getIElement elements (+ c1 1))) (append preCS (list preCard)) elements)
                    (cardsSet->generateN2CardsAux c1 c2 (+ c3 1) n (card->addSymbol preCard (cardsSet->getIElement elements (+ n 2 (* n (- c3 1))(modulo(+(*(- c1 1)(- c3 1))(- c2 1)) n)))) preCS elements)
                )
            )
        )
    )
)

;Dom: list x int
;Rec: element (depende mucho del tipo de datos con el que se construya el cardsSet)
;Deuvelve el iesimo elemento de una lista
(define cardsSet->getIElement (lambda (elements i)
        (cardsSet->getIElementAux 1 elements i)
    )
)

;Dom: int x list x int
;Rec: element (depende mucho del tipo de datos con el que se construya el cardsSet)
;Deuvelve el iesimo elemento de una lista
(define cardsSet->getIElementAux (lambda(counter elements i)
        (if (= counter i)
            (car elements)
            (cardsSet->getIElementAux (+ counter 1) (cdr elements) i)
        )
    )
)

;Dom: list(card) x int x int
;Rec: list(card)
;Devuelve las primeras N cartas de un conjunto utilizando recursion de cola
(define cardsSet->getNCards (lambda (CS maxC newCS)
        (if (or (=(length newCS) maxC) (= 0 (length CS)))
            newCS
            (cardsSet->getNCards (cdr CS) maxC (append newCS (list (car CS))))
        )
    )
)

;Dom: list(card)
;Rec: Bool
;Verifica si cada una de las cartas tiene en su interior solo simbolos distintos utilizando recursion de cola
(define cardsSet->allCardsJustDifferentSymbols (lambda (cartas)
        (if (null? cartas)
            #t
            (if (card->JustDifferentSymbols (car cartas))
                (cardsSet->allCardsJustDifferentSymbols (cdr cartas)) 
                #f
            )
        )
    )
)

;Dom: list(card)
;Rec: Bool
;Verifica que cada carta coincida con las demas solo en un simbolo utilizando recursion de cola
(define cardsSet->allCardsJust1Match(lambda (cartas)
        (if (null? cartas)
            #t
            (if (cardsSet->allCardsJust1MatchAux (car cartas)(cdr cartas))
                (cardsSet->allCardsJust1Match (cdr cartas))
                #f
            )
        )
    )
)

;Dom: card x list(card) 
;Rec: Bool
;Verifica que una carta coincida solo en un simbolo con las demas cartas de una lista utilizando recursion
;de cola
(define cardsSet->allCardsJust1MatchAux (lambda (carta cartas)
        (if (null? cartas)
            #t
            (if (card->just1Match carta (car cartas))
                (cardsSet->allCardsJust1MatchAux carta (cdr cartas))
                #f
            )
        )
    )
)


;Dom: cardsSet x int
;Rec: card
;Devuelve la enesima carta de un conjunto
(define cardsSet->nthCard(lambda (CS n)
        (define cartas (lazy (filter (lambda (carta) (= (obtenerPosicion (cardsSet->getCartas CS) carta) n))(cardsSet->getCartas CS))))
        (if (cardsSet->dobble? CS)
            (if (null? (force cartas))
                null
                (car (force cartas))
            )
            null
        )
    )
)

;Dom: card
;Rec: int
;Retorna la cantidad de cartas totales de un conjunto valido a partir de una carta de prueba
(define cardsSet->findTotalCards (lambda (carta)
        (define n (lazy (cardsSet->order (length carta))))
        (if (card? carta)
            (+ (* (force n) (force n)) (force n) 1)
            0
        )
    )
)
;Dom: card
;Rec: int
;Retorna la cantidad de simbolos necesarios para generar un conjunto valido
(define cardsSet->requiredElements cardsSet->findTotalCards)


;Dom: cardsSet
;Rec: cardsSet
;Retorna las cartas faltantes del conjunto
(define cardsSet->missingCards (λ (CS)
        (if (cardsSet->dobble? CS)
            (filter (λ (carta) (not(isIn? (cardsSet->getCartas CS) carta))) (cardsSet->getCartas (cardsSet (cardsSet->getElements CS) (length(cardsSet->getFirstCard CS)) 0 (cardsSet->getRndFn CS))))
            null
        )
    )
)

;Dom: cardsSet
;Rec: string
;Convierte el cardsSet en una representacion en string entendible para un usuario
(define cardsSet->string (λ (CS)
        (if (cardsSet->dobble? CS)
            (string-append 
                "CARTAS EN EL CONJUNTO: " (~s (cardsSet->getCantCartas CS)) "\n"   
                "ELEMENTOS QUE FORMAN LAS CARTAS " (~s (cardsSet->getElements CS)) "\n" 
                "CARTAS:\n" (string-join (map (λ (carta) (~s carta)) (cardsSet->getCartas CS))"\n")
            )
            "EL CONJUNTO INGRESADO NO ES VALIDO"
        )
    )
)