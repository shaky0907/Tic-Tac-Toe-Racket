#lang racket
(require 2htdp/universe)
(provide main)
(provide initialize display-moves)
(require 2htdp/image)


;;GUI variables
(define size null)
(define box null)
(define boxes null)
(define display-text (text "Hello" 24 "orange"))
(define text-area null)
(define ttt-brd null)
(define n-board null)
(define m-board null)
(define player-first (list 'p '() '() '(1 2 3 4 5 6 7 8 9)))



(define (create-boxes n)
  (set! size 150)
  
  (cond[(< n 6)(set! box (rectangle size size 'outline 'black))]
       [else (set! size 80)(set! box (rectangle size size 'outline 'black))])
  (cond[(= n 3) (set! boxes (beside box box box))]
       [(= n 4) (set! boxes (beside box box box box))]
       [(= n 5) (set! boxes (beside box box box box box))]
       [(= n 6) (set! boxes (beside box box box box box box))]
       [(= n 7) (set! boxes (beside box box box box box box box))]
       [(= n 8) (set! boxes (beside box box box box box box box box box))]
       [(= n 9) (set! boxes (beside box box box box box box box box box box))]
       [(= n 10) (set! boxes (beside box box box box box box box box box box))]))




(define (create-ttt-board n m)
  (set! text-area (rectangle (* n size) (/ size m)  'outline 'white))
  (set! m-board m)
  (set! n-board n)
  (cond[(= m 3) (set! ttt-brd (above boxes boxes boxes text-area))]
       [(= m 4) (set! ttt-brd (above boxes boxes boxes boxes text-area))]
       [(= m 5) (set! ttt-brd (above boxes boxes boxes boxes boxes text-area))]
       [(= m 6) (set! ttt-brd (above boxes boxes boxes boxes boxes boxes text-area))]
       [(= m 7) (set! ttt-brd (above boxes boxes boxes boxes boxes boxes boxes text-area))]
       [(= m 8) (set! ttt-brd (above boxes boxes boxes boxes boxes boxes boxes boxes text-area))]
       [(= m 9) (set! ttt-brd (above boxes boxes boxes boxes boxes boxes boxes boxes boxes text-area))]
       [(= m 10) (set! ttt-brd (above boxes boxes boxes boxes boxes boxes boxes boxes boxes boxes text-area))]))



(define (sq-num n m contador1 contador2 contador3)
  (cond [(and (equal? n contador1)
              (equal? m contador2)) contador3]
        [(equal? contador2 (+ 1 m-board)) 0]
        [(equal? contador1 n-board)(sq-num n m 1 (+ 1 contador2) (+ 1 contador3))]
        [else(sq-num n m (+ 1 contador1) contador2 (+ 1 contador3))]))


(define (display-moves brd x y me)
  (local (
          ;; Nat -> Board
          ;; makes a new board
          (define (create-board n)
            (cond [(member? n (fourth brd))
                  (list 'c
                  (second brd)
                  (append (third brd) (list n))
                  (remove n (fourth brd)))]
                  [else brd]))
          
          ;; Get what square the click was made
          (define (get-sq)
             (cond[(string=? me "button-down") (sq-num (ceiling(/ x size)) (ceiling(/ y size)) 1 1 1)]
                  [else 0])))
    
          
          ;;(print (get-sq))
    (cond
      [(not(equal? 0 (get-sq)))(create-board (get-sq))]
      [else brd])))

(define (initialize brd)
  (place-image
   (text (cond[(symbol=? 'p (first brd))  "Your Turn"]
              [else "Computer thinking"])
         (floor (/ 100 6)) 'green)
   (floor (/(* n-board size) 2))
   (floor (+ (* ( + m-board 0.17) size) ))
   (foldr p-moves
          (foldr c-moves ttt-brd (second brd))
          (third brd))))


;; Nat Img -> Img
(define (c-moves n im)
  (place-image (text "O" (/ size 2) 'blue)
               (first (gridsq n))
               (second (gridsq n))
               im))

;; Num Img -> Img
(define (p-moves n im)
  (place-image (text "X" (/ size 2) 'red)
               (first (gridsq n))
               (second (gridsq n))
               im))


(define (gridsq_aux n contador1 contador2 contador3)
  (cond [(equal? (+ m-board 1) contador2) (list 0 0)]
        [(equal? n contador3) (list contador1 contador2)]
        [(equal? contador1 n-board) (gridsq_aux n 1 (+ contador2 1) (+ contador3 1))]
        [else (gridsq_aux n (+ contador1 1) contador2 (+ contador3 1))]))


;; Nat -> (listof Num Num)
(define (gridsq n)
  (grid (first (gridsq_aux n 1 1 1)) (second (gridsq_aux n 1 1 1))))


(define (grid x y)
  (list (* size (- x .5)) (* size (- y .5))))



(define (main brd n m)
  (cond [(> n 10) 'Tamaño_invalido]
        [(> m 10) 'Tamaño_invalido]
        [else (create-boxes n)(create-ttt-board n m)
         (big-bang brd
            (to-draw initialize)
            (on-mouse display-moves))]))



(define (member? x lst) (not (false? (member x lst))))