#lang racket
(provide (all-defined-out))
(require "card.rkt")
(require "../functions.rkt")

;Representacion
;cardsSet = CantidadCartas X Cartas X Elements

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
(define cardsSet->getCantCartas car)
(define cardsSet->getCartas cadr)
(define cardsSet->getRndFn (λ (CS) (caddr (cdr CS))))
(define cardsSet->getFirstCard(lambda (CS)  
        (car (cardsSet->getCartas CS))    
    )
)
(define cardsSet->getNextCards (lambda (CS)  
        (cdr (cardsSet->getCartas CS))    
    )
)

(define cardsSet->getElements caddr)


(define cardsSet->dobble? (lambda (CS)
        (if (and (cardsSet->allCardsJustDifferentSymbols (cardsSet->getCartas CS)) (cardsSet->allCardsJust1Match (cardsSet->getCartas CS)))
            (isPowerOfPrime (cardsSet->order (length(cardsSet->getFirstCard CS))))
            #f
        )
    )
)

;------------Funciones propias------------

;Funcion que retorna el orden del plano proyectivo del conjunto
;Dom: int (numero de elementos de una carta del conjunto)
;Rec: int (orden n)

(define cardsSet->order (lambda (numE)
        (- numE 1)
    )
)

(define cardsSet->generateFirstCard (lambda (n preCS elements)
        (append preCS (list (cardsSet->generateFirstCardAux 1 n null elements)))
    )
)

(define cardsSet->generateFirstCardAux(lambda (counter n preCard elements)
        (if (= (length preCard) (+ n 1))
            preCard
            (cardsSet->generateFirstCardAux (+ 1 counter) n (card->addSymbol preCard (cardsSet->getIElement elements counter)) elements)
            
        )
    )
)

(define cardsSet->generateNCards (lambda(n preCS elements)
        (append preCS (cardsSet->generateNCardsAux 1 1 n (card 1 (cardsSet->getIElement elements 1)) null elements))
    )
)

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

(define cardsSet->generateN2Cards (lambda (n preCS elements)
        (append preCS (cardsSet->generateN2CardsAux 1 1 1 n (card 1 (cardsSet->getIElement elements 2)) null elements))
    )
)

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

(define cardsSet->getIElement (lambda (elements i)
        (cardsSet->getIElementAux 1 elements i)
    )
)
(define cardsSet->getIElementAux (lambda(counter elements i)
        (if (= counter i)
            (car elements)
            (cardsSet->getIElementAux (+ counter 1) (cdr elements) i)
        )
    )
)

(define cardsSet->getNCards (lambda (CS maxC newCS)
        (if (or (=(length newCS) maxC) (= 0 (length CS)))
            newCS
            (cardsSet->getNCards (cdr CS) maxC (append newCS (list (car CS))))
        )
    )
)

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

(define cardsSet->numCards (lambda(CS)
        (if (cardsSet->dobble? CS)
            (length(cardsSet->getCartas CS))
            0
        )
    )
)

(define cardsSet->nthCard(lambda (CS n)
        (define cartas (lazy (filter (lambda (carta) (= (obtenerPosicion (cardsSet->getCartas CS) carta) n))(cardsSet->getCartas CS))))
        (if (cardsSet->dobble? CS)
            (if (null? (force cartas))
                null
                (car (force cartas))
            )
            #f
        )
    )
)

(define cardsSet->findTotalCards (lambda (carta)
        (define n (lazy (cardsSet->order (length carta))))
        (if (card? carta)
            (+ (* (force n) (force n)) (force n) 1)
            0
        )
    )
)

(define cardsSet->requiredElements cardsSet->findTotalCards)

(define cardsSet->missingCards (λ (CS)
        (if (cardsSet->dobble? CS)
            (filter (λ (carta) (not(isIn? (cardsSet->getCartas CS) carta))) (cardsSet->getCartas (cardsSet (cardsSet->getElements CS) (length(cardsSet->getFirstCard CS)) 0 (cardsSet->getRndFn CS))))
            null
        )
    )
)

(define cardsSet->String (λ (CS)
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