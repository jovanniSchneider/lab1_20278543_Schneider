#lang racket
(provide (all-defined-out))

;En este archivo se encuentran las funciones que no se consideran propias a ningun TDA

;Dom: un entero
;Rec: un booleano
;Verifica si un numero es primo
(define isPrime (lambda (n)
        (if (<= n 1)
            #f
            (isPrimeAux n 2)
        )
    )
)
;Dom: 2 enteros
;Rec: un booleano
;Verifica que no haya un divisor desde 2 hasta n, lo que indicaria que es primo (utiliza recursion de cola)
(define isPrimeAux (lambda(n divisor)
        (if (= divisor n)
            #t
            (if (= (modulo n divisor) 0)
                #f
                (isPrimeAux n (+ divisor 1))
            )
        )
    )
)

;Dom: un entero
;Rec: una lista
;Entrega una lista con los numeros primos hasta n
(define primesToN (lambda(n)
        (if (<= n 1)
            null
            (primesToNAux n 2 null)
        )
    )
)

;Dom: 2 enteros y una lista inicialmente vacia
;Rec: una lista
;Busca los numeros primos hasta n usando recursion de cola
(define primesToNAux (lambda (n counter lista)
        (if (= counter n)
            (if (isPrime counter)
                (append lista (list counter))
                lista
            )
            (if (isPrime counter)
                (primesToNAux n (+ counter 1) (append lista (list counter)))
                (primesToNAux n (+ counter 1) lista)
            )
        )
    )
)

;Dom: 2 enteros
;Rec: un booleano
;Verifica si a es potencia de b utilizando recursion de cola
(define isPowerOf (lambda (a b)
        (if (= a 1)
            #t
            (if (= (modulo a b) 0)
                (isPowerOf (/ a b) b)
                #f
            )
        )
    )
)

(define isPowerOfPrime (lambda(n)
        (not(null?(filter(lambda (primo) (isPowerOf n primo)) (primesToN n))))
    )
)

(define obtenerPosicion (lambda (lista elemento)
        (obtenerPosicionAux lista elemento 1)
    )
)

(define obtenerPosicionAux (lambda (lista elemento contador)
        (if (null? lista)
            -1
            (if (equal? (car lista) elemento)
                contador
                (obtenerPosicionAux (cdr lista) elemento (+ 1 contador))
            )
        )
    )
)

(define isIn? (λ (lista elemento)
        (not(null? (filter (λ (listElement) (equal? listElement elemento)) lista)))
    )
)