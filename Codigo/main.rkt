#lang racket
(require "TDAs/cardsSet.rkt")
(require "TDAs/card.rkt")
(require "functions.rkt")
(require "TDAs/game.rkt")

;A continuacion se listan ejemplos de los requerimientos funcionales
;Los ejemplos que estan comentados se deben copiar y pegar en la consola de ejecucion


;-----cardsSet-constructor------
    (define CS (cardsSet (list 1 2 3 4 5 6 7 8 9 10 11 12 13) 4 7 randomFn))
    (define CS2 (cardsSet (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21) 5 7 randomFn))
    (define noCS (cardsSet (list 1 2 3 4 5 6 7 8 10 10 11 12 13) 4 7 randomFn))

;-----cardsSet-dobble?-------
    ;(cardsSet->dobble? CS)
    ;(cardsSet->dobble? CS2)
    ;(cardsSet->dobble? noCS)

;-----cardsSet-nthCard------
    ;(cardsSet->nthCard CS 4)
    ;(cardsSet->nthCard noCS 4)
    ;(cardsSet->nthCard CS2 7)

;-----cardsSet-findTotalCards-----
    ;(cardsSet->findTotalCards (cardsSet->nthCard CS 4))
    ;(cardsSet->findTotalCards (cardsSet->nthCard CS2 3))
    ;(cardsSet->findTotalCards (cardsSet->nthCard noCS 7))

;-----cardsSet-requiredElements-----
    ;(cardsSet->requiredElements (cardsSet->nthCard CS 4))
    ;(cardsSet->requiredElements (cardsSet->nthCard CS2 4))
    ;(cardsSet->requiredElements (cardsSet->nthCard noCS 4))

;-----cardsSet-missingCards-----
    ;(cardsSet->missingCards CS)
    ;(cardsSet->missingCards CS2)
    ;(cardsSet->missingCards noCS)

;Idealmente pasar los siguientes ejemplos a la funcion display
;-----cardsSet->string------
    ;(cardsSet->string CS)
    ;(cardsSet->string CS2)
    ;(cardsSet->string noCS)

;-----game constructor-----
    (define g (game 2 CS game->stackMode randomFn))
    (define g0 (game 4 CS2 game->stackMode randomFn))
    (define noG (game 3 noCS game->stackMode randomFn))

;-----game-stackMode-----
    ;(game->stackMode CS)
    ;(game->stackMode CS2)
    ;(game->stackMode noCS)

;-----game-register-----
    (define g1 (game->register g "jovanni"))
    (define g2 (game->register g1 "pepe"))
    (define g3 (game->register g2 "sandra"))

