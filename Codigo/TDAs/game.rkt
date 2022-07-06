#lang racket
(provide (all-defined-out))
(require "../functions.rkt")
(require "cardsSet.rkt")


(define game (λ (numP CS mode rndFn)
        (if (and (and(number? numP)(> numP 0)) (cardsSet->dobble? CS) (procedure? mode)(procedure? rndFn))
            (list numP null CS mode "")
            null
        )
    )
)

(define game? (λ (g)
        (and (and(number? (car g))(> (car g) 0)) (cardsSet->dobble? (caddr g)) (procedure? (caddr (cdr g)))(<= (length (cadr g)) (car g)))
    )
)

(define game->getCantPlayers car)
(define game->getPlayers cadr)
(define game->getCardsSet caddr)
(define game->getMode (λ (g) (caddr (cdr g))))
(define game->getTurn (λ (g) (caddr (cdr(cdr g)))))

(define game->register (λ (g player)
        (if (game? g)
            (if (and (string? player) (not (isIn? (game->getPlayers g) player)))
                (if (<= (+ (length (game->getPlayers g)) 1) (game->getCantPlayers g))
                    (list (game->getCantPlayers g)(append (game->getPlayers g) (list player))(game->getCardsSet g)(game->getMode g)(game->getTurn g))
                    g
                )
                g
            )
            null
        )
    )
)