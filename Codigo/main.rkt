#lang racket
(require "TDAs/cardsSet.rkt")
(require "TDAs/card.rkt")
(require "functions.rkt")
(require "TDAs/game.rkt")

;A continuacion se listan ejemplos de los requerimientos funcionales

;-----cardsSet-constructor------
(define CS (cardsSet (list 1 2 3 4 5 6 7 8 9 10 11 12 13) 4 7 randomFn))
(define CS2 (cardsSet (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21) 5 -1 randomFn))
(define noCS (cardsSet (list 1 2 3 4 5 6 7 8 10 10 11 12 13) 4 7 randomFn))
;-----cardsSet-dobble?-------
;(cardsSet->dobble? CS)
;(cardsSet->dobble? CSchar)
;(cardsSet->dobble? noCS)
(define g (game 3 CS randomFn randomFn))
(define g1 (game->register g "jovanni"))
(define g2 (game->register g1 "pepe"))
(define g3 (game->register g2 "sandra"))