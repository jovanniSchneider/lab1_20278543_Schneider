#lang racket
(provide (all-defined-out))
(require "../functions.rkt")
(require "cardsSet.rkt")

;Representacion
;Game = cantJugadores(int) x jugadores(list) x cardsSet x mode(procedure) x turn(string)

;---------Constructor-----------
;Dom: int x cardsSet x procedure x procedure
;Rec: game
(define game (λ (numP CS mode rndFn)
        (if (and (and(number? numP)(> numP 0)) (cardsSet->dobble? CS) (procedure? mode)(procedure? rndFn))
            (list numP null CS mode "")
            null
        )
    )
)

;----------Pertenencia----------
;Dom: game
;Rec: bool
(define game? (λ (g)
        (and (and(number? (car g))(> (car g) 0)) (cardsSet->dobble? (caddr g)) (procedure? (caddr (cdr g)))(<= (length (cadr g)) (car g)))
    )
)


;-------Getters-------
;Dom: game
;Rec: int
;Devuelve la cantidad de jugadores del game
(define game->getCantPlayers(λ(g)
        (if(game? g)
            (car g)
            null
        )
    )
)
;Dom: game
;Rec: list(string)
;Devuelve los jugadores del game
(define game->getPlayers (λ(g)
        (if(game? g)
            (cadr g)
            null
        )
    )
)
;Dom: game
;Rec: cardsSet
;Devuelve el cardsSet del game
(define game->getCardsSet (λ(g)
        (if(game? g)
            (caddr g)
            null
        )
    )
)

;Dom: game
;Rec: procedure
;Devuelve el modo de juego del game
(define game->getMode (λ (g) 
        (if (game? g)
            (caddr (cdr g))
            null
        )
    )
)

;Dom: game
;Rec: string
;Devuelve el turno correspondiente del game
(define game->getTurn (λ (g) 
        (if (game? g)
            (caddr (cdr(cdr g)))
            null
        )    
    )
)

;------------Funciones propias------------

;Dom: game x string
;Rec: game
;Registra un jugador en el juego, solo si el jugador no estaba registrado antes y no se ha alcanzado el
;limite de jugadores
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

;Dom: cardsSet
;Rec: list(card)
;Retira las 2 primeras cartas del cardsSet y las devuelve
(define game->stackMode (λ(CS)
        (if (cardsSet->dobble? CS)
            (filter (λ (carta) 
                        (define index (lazy (obtenerPosicion (cardsSet->getCartas CS) carta)))
                        (or(= (force index) 1)(= (force index) 2))
                )
                (cardsSet->getCartas CS)
            )
            null
        )
    )
)