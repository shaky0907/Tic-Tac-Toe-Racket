#lang racket
(require 2htdp/universe)
(provide main)
(provide initialize display-moves)
(require 2htdp/image)
(require "algorithms.rkt")

;GUI variables
(define size null); tamaño de cajas
(define box null) ; Definición de las casillas
(define boxes null); Definición de las filas
(define text-area null) ; Definición de area de texto
(define ttt-brd null); Definición del tablero
(define n-board null); Definición cantidad de columnas
(define m-board null); Definición cantidad de filas


; Genera las casillas : n cantidad de columnas
(define (create-boxes n)
  (set! size 150);Set box size
  
  (cond[(< n 5)(set! box (rectangle size size 'outline 'black))];ajustar tamaño dependiendo del tablero
       [(< n 8)(set! size 100)(set! box (rectangle size size 'outline 'black))]
       [else (set! size 60)(set! box (rectangle size size 'outline 'black))])
  
  (cond[(= n 3) (set! boxes (beside box box box))]
       [(= n 4) (set! boxes (beside box box box box))]
       [(= n 5) (set! boxes (beside box box box box box))]
       [(= n 6) (set! boxes (beside box box box box box box))]
       [(= n 7) (set! boxes (beside box box box box box box box))]
       [(= n 8) (set! boxes (beside box box box box box box box box box))]
       [(= n 9) (set! boxes (beside box box box box box box box box box box))]
       [(= n 10) (set! boxes (beside box box box box box box box box box box))]))



; Genera el tablero: n columnas, m filas.
(define (create-ttt-board n m)
  (set! text-area (rectangle (* n size) (/ 200 m)  'outline 'white)) ; Set text box area
  (set! m-board m) ; set filas
  (set! n-board n) ; set columnas
  
  (cond[(= m 3) (set! ttt-brd (above boxes boxes boxes text-area))]
       [(= m 4) (set! ttt-brd (above boxes boxes boxes boxes text-area))]
       [(= m 5) (set! ttt-brd (above boxes boxes boxes boxes boxes text-area))]
       [(= m 6) (set! ttt-brd (above boxes boxes boxes boxes boxes boxes text-area))]
       [(= m 7) (set! ttt-brd (above boxes boxes boxes boxes boxes boxes boxes text-area))]
       [(= m 8) (set! ttt-brd (above boxes boxes boxes boxes boxes boxes boxes boxes text-area))]
       [(= m 9) (set! ttt-brd (above boxes boxes boxes boxes boxes boxes boxes boxes boxes text-area))]
       [(= m 10) (set! ttt-brd (above boxes boxes boxes boxes boxes boxes boxes boxes boxes boxes text-area))]))


; Determina cual casilla se selecciono:
;n columnas
;m filas
;contador1 1 columnas
;contador2 1 filas
;contador3 1 numero de casilla
(define (sq-num n m contador1 contador2 contador3)
  (cond [(and (equal? n contador1)
              (equal? m contador2)) contador3]
        [(equal? contador2 (+ 1 m-board)) 0]
        [(equal? contador1 n-board)(sq-num n m 1 (+ 1 contador2) (+ 1 contador3))]
        [else(sq-num n m (+ 1 contador1) contador2 (+ 1 contador3))]))



; Se encarga de manejar registrar movimientos:
;brd movimentos (jugador c-moves p-moves r-moves)
;x posición horizontal mouse
;y posición vertical mouse
;me acción del mouse
(define (display-moves brd x y me)
  (local (
          ;Revisar si se hizo click en una casilla valida
          (define (get-sq)
             (cond[(and (string=? me "button-down")(not(gameover? brd))) (sq-num (ceiling(/ x size)) (ceiling(/ y size)) 1 1 1)]
                  [else 0])))
   
    (cond
      [(not(equal? 0 (get-sq)))(register-move (get-sq) brd n-board m-board)];;comprueba movimiento y lo realiza
      [else brd])))



;Dibuja el tablero: brd movimientos
(define (initialize brd)
  (place-image
   ;Texto dependiendo de condicion:
   (text (cond[(and(symbol=? 'p (first brd))(not(gameover? brd)))  "Your Turn"]; cuando no se hace movimiento
              [(and(symbol=? (who-won brd)'c)
                   (gameover? brd)) "Computer wins"]; cuando computadora gana
              [(and (symbol=? (who-won brd) 'p)
                    (gameover? brd)) "Player wins"]; cuando jugador gana
              [(and (gameover? brd)
                    (symbol=? (who-won brd) 'x)) "Draw"]; cuando se empata
              [else "Computer thinking"]); Turno de computadora
         (floor (/ 150 6)) 'black)
   (floor (/(* n-board size) 2))
   (floor (+ (* ( + m-board 0.20) size) ))
   (foldr p-moves
          (foldr c-moves ttt-brd (second brd))
          (third brd))))


; Dibuja los movimientos de la computadora
(define (c-moves n im)
  (place-image (text "O" (/ size 2) 'blue)
               (first (gridsq n))
               (second (gridsq n))
               im))

; Dibuja los movimientos del jugador
(define (p-moves n im)
  (place-image (text "X" (/ size 2) 'red)
               (first (gridsq n))
               (second (gridsq n))
               im))

;Determina las coordenadas en el tablero con el numero de casilla:
;n numero de casilla
;contador1 1 columnas
;contador2 1 filas
;contador3 1 numero de casilla
(define (gridsq_aux n contador1 contador2 contador3)
  (cond [(equal? (+ m-board 1) contador2) (list 0 0)]
        [(equal? n contador3) (list contador1 contador2)]
        [(equal? contador1 n-board) (gridsq_aux n 1 (+ contador2 1) (+ contador3 1))]
        [else (gridsq_aux n (+ contador1 1) contador2 (+ contador3 1))]))


; Retorna las coordenadas: n numero de casilla
(define (gridsq n)
  (grid (first (gridsq_aux n 1 1 1)) (second (gridsq_aux n 1 1 1))))


;Transfroma coordenadas de casillas a coordenadas de la pantalla
(define (grid x y)
  (list (* size (- x .5)) (* size (- y .5))))



;Determina quien gano: brd movimientos (jugador c-moves p-moves r-moves)
(define (who-won brd)
  (cond [(and(symbol=? (first brd) 'p)
             (equal? (fifth brd) #t)) 'c]
        [(and(symbol=? (first brd) 'c)
             (equal? (fifth brd) #t)) 'p]
        [else 'x]))

;Determina si el juego se acabo: brd movimientos (jugador c-moves p-moves r-moves)
(define (gameover? brd)
  (cond[(or(empty? (fourth brd))
           (equal? (fifth brd) #t))]
       [else #f]))

;realiza el turno de la computadora:  brd movimientos (jugador c-moves p-moves r-moves)
(define (c-turn brd)
  (cond [(gameover? brd) brd]
        [(not(player-turn? (first brd))) (candidates brd n-board m-board)]
        [else brd]))


;Funcion principal:
;brd movimientos (jugador c-moves p-moves r-moves)
;n columnas
;m filas
(define (main brd n m)
  (cond [(> n 10) 'Tamaño_invalido]
        [(> m 10) 'Tamaño_invalido]
        [else (create-boxes n)(create-ttt-board n m)
         (big-bang brd
            (name 'TicTacToe)
            (to-draw initialize)
            (on-mouse display-moves)
            (on-tick c-turn))]))


;revisa si elemento pertence a la lista:
;x elemento
;lst lista
(define (member? x lst) (not (false? (member x lst))))

;Retorna la lista de movimentos posibles:
;n numero de casilla
;m numero de filas
;contador1 1 columnas
;contador2 1 filas
;contador3 1 numero de casilla
;lst lista de movimientos
(define (give-list n m contador1 contador2 contador3 lst)
  (cond[(equal? contador2 (+ 1 m)) lst]
       [(equal? contador1 n)(give-list n m 1 (+ 1 contador2) (+ 1 contador3) (append lst (list contador3)))]
       [else(give-list n m (+ 1 contador1) contador2 (+ 1 contador3) (append lst (list contador3)))]))



;Funcion del juego
(define (TTT n m)
  
  (main (list 'p '() '() (give-list n m 1 1 1 '()) #f) n m))