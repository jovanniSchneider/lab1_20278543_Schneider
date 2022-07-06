#lang racket
(require "TDAs/cardsSet.rkt")
(require "TDAs/card.rkt")
(require "functions.rkt")
(require "TDAs/game.rkt")
;Funcion seudoaleatoria
;Dom: int(seed)
;Rec: int
(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (modulo (+ (* a xn) c) m) 3)
                 )
)
(define CS (cardsSet (list 1 2 3 4 5 6 7 8 9 10 11 12 13) 4 7 randomFn))
(define noCS (cardsSet (list 1 2 3 4 5 6 7 8 10 10 11 12 13) 4 7 randomFn))
(define g (game 3 CS randomFn randomFn))
(define g1 (game->register g "jovanni"))
(define g2 (game->register g1 "pepe"))
(define g3 (game->register g2 "sandra"))
;Falta el missing cards