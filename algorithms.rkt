#lang racket
;; A Module for basic functions [description of module]

(provide player-turn? win-move? register-move candidates selection viability depth)

;; (player-turn? turn) produces true if it is player's turn and false otherwise
;; player-turn?: Sym -> Bool

;; (game-over? board) produces true if game is over, false otherwise
;; game-over?: Board -> Bool

;; (win? moves) checks if 'moves' is a winning combination
;; win?: (listof Nat) -> Bool

;; (register-move move board) registers 'move' in brd and produces the
;; board after the move has been made
;; register-move: Nat Board -> Board

;; (poss-brds board) produces a list of Boards that are one move from
;; the current Board
;; poss-brds: Board -> (listof Board)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IMPLEMENTATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Board is a (list Sym (listof Nat) (listof Nat) (listof Nat))
;; first -> Who's turn it is to play
;; second -> computer's moves
;; third -> player's moves
;; fourth -> remaining moves

;;


;;Quicksorts a given list of numbers
(define (quicksort lista)
  (cond
    [(null? lista) lista]
    [else (quicksortaux (cdr lista) (car lista) (list) (list))]))

(define (quicksortaux lista pivote mayor menor)
  (cond
    [(null? lista) (append (append(quicksort menor) (list pivote))(quicksort mayor))]
    [(>= (car lista) pivote) (quicksortaux (cdr lista) pivote (cons (car lista) mayor) menor)]
    [(< (car lista) pivote) (quicksortaux (cdr lista) pivote mayor (cons (car lista) menor))]))

;;Returns length of a given list
(define (list-length lst)
  (cond [(empty? lst) 0]
        [else (+ 1 (list-length (cdr lst)))]))
  

;;Returns list of all right available spaces
(define (check-right contador1 contador2 contador3 cols rows lst)
  (cond [(equal? contador2 (+ rows 1)) lst]
        [(equal? contador1 cols) (check-right 3 (+ contador2 1) (+ contador3 3) cols rows (append lst (list contador3)))]
        [else (check-right (+ contador1 1) contador2 (+ contador3 1) cols rows (append lst (list contador3)))]))

;;Returns list of all left available spaces
(define (check-left contador1 contador2 contador3 cols rows lst)
  (cond [(equal? contador2 (+ rows 1)) lst]
        [(equal? contador1 (- cols 2)) (check-left 1 (+ contador2 1) (+ contador3 3) cols rows (append lst (list contador3)))]
        [else (check-left (+ contador1 1) contador2 (+ contador3 1) cols rows (append lst (list contador3)))]))

;;Returns list of all middle available spaces
(define (check-middle contador1 contador2 contador3 cols rows lst)
  (cond [(equal? contador2 (+ rows 1)) lst]
        [(equal? contador1 (- cols 1)) (check-middle 2 (+ contador2 1) (+ contador3 3) cols rows (append lst (list contador3)))]
        [else (check-middle (+ contador1 1) contador2 (+ contador3 1) cols rows (append lst (list contador3)))]))

;;Returns list of left column
(define (get-left contador1 contador2 contador3 cols rows lst)
  (cond [(equal? contador2 (+ rows 1)) lst]
        [(equal? contador1 1) (get-left 1 (+ contador2 1) (+ contador3 cols) cols rows (append lst (list contador3)))]))

;;Returns list of right column
(define (get-right contador1 contador2 contador3 cols rows lst)
  (cond [(equal? contador2 (+ rows 1)) lst]
        [(equal? contador1 cols) (get-right cols (+ contador2 1) (+ contador3 cols) cols rows (append lst (list contador3)))]))

;;Check three vertical with last move on bottom
(define (win-state-1? move brd cols rows)
  (cond[(and (>= move (+ (* 2 cols) 1))
             (member? (- move cols) brd)
             (member? (- move (* cols 2)) brd))#t]
       [else #f]))

;;Check three vertical with last move on top
(define (win-state-2? move brd cols rows)
  (cond[(and (<= move (- (* cols rows) (* cols 2)))
             (member?(+ move cols) brd)
             (member? (+ move (* cols 2)) brd)) #t]
       [else #f]))

;;Check three vertical with last move on middle
(define (win-state-3? move brd cols rows)
  (cond[(and (>= move (+ 1 cols))
             (<= move (- (* cols rows) cols))
             (member? (- move cols) brd)
             (member? (+ move cols) brd)) #t]
       [else #f]))

;;Check three horizontal with last move on right
(define (win-state-4? move brd cols rows)
  (cond[(and (member? move (check-right '3 '1 '3 cols rows '()))
             (member? (- move 1) brd)
             (member? (- move 2) brd)) #t]
       [else #f]))

;;Check three horizontal with last move on left
(define (win-state-5? move brd cols rows)
  (cond[(and (member? move (check-left '1 '1 '1 cols rows '()))
             (member? (+ move 1) brd)
             (member? (+ move 2) brd)) #t]
       [else #f]))

;;Check three horizontal with last move on middle
(define (win-state-6? move brd cols rows)
  (cond[(and (member? move (check-middle '2 '1 '2 cols rows '()))
             (member? (- move 1) brd)
             (member? (+ move 1) brd)) #t]
       [else #f]))

;;Check three diagonal with last move on bottom right
(define (win-state-7? move brd cols rows)
  (cond[(and (member? move (check-right '3 '1 '3 cols rows '()))
             (>= move (+ (* 2 cols) 1))
             (member? (- move (+ cols 1)) brd)
             (member? (- move (+ (* cols 2) 2)) brd)) #t]
       [else #f]))

;;Check three diagonal with last move on bottom left
(define (win-state-8? move brd cols rows)
  (cond[(and (member? move (check-left '3 '1 '3 cols rows '()))
             (>= move (+ (* 2 cols) 1))
             (member? (- move (- cols 1)) brd)
             (member? (- move (- (* cols 2) 2)) brd)) #t]
       [else #f]))

;;Check three diagonal with last move on top right
(define (win-state-9? move brd cols rows)
  (cond[(and (member? move (check-right '3 '1 '3 cols rows '()))
             (<= move (- (* cols rows) (* cols 2)))
             (member? (+ move (- cols 1)) brd)
             (member? (+ move (- (* cols 2) 2)) brd)) #t]
       [else #f]))

;;Check three diagonal with last move on top left ERROR
(define (win-state-10? move brd cols rows)
  (cond[(and (member? move (check-left '1 '1 '1 cols rows '()))
             (<= move (- (* cols rows) (* cols 2)))
             (member? (+ move (+ cols 1)) brd)
             (member? (+ move (+ (* cols 2) 2)) brd)) #t]
       [else #f]))

;;Check three diagonal from top left to bottom right with last move on middle
(define (win-state-11? move brd cols rows)
  (cond[(and (member? move (check-middle '2 '1 '2 cols rows '()))
             (>= move (+ 1 cols))
             (<= move (- (* cols rows) cols))
             (member? (- move (+ cols 1)) brd)
             (member? (+ move (+ cols 1)) brd)) #t]
       [else #f]))

;;Check three diagonal from top right to bottom left with last move on middle
(define (win-state-12? move brd cols rows)
  (cond[(and (member? move (check-middle '2 '1 '2 cols rows '()))
             (>= move (+ 1 cols))
             (<= move (- (* cols rows) cols))
             (member? (- move (- cols 1)) brd)
             (member? (+ move (- cols 1)) brd)) #t]
       [else #f]))

;;Checks all above win-states for memory usage reduction
(define (win-state? move brd cols rows)
  (define lst1 (check-right '3 '1 '3 cols rows '()))
  (define lst2 (check-left '1 '1 '1 cols rows '()))
  (define lst3 (check-middle '2 '1 '2 cols rows '()))
  (cond [(or (and (>= move (+ (* 2 cols) 1))
                  (member? (- move cols) brd)
                  (member? (- move (* cols 2)) brd))
             (and (<= move (- (* cols rows) (* cols 2)))
                  (member?(+ move cols) brd)
                  (member? (+ move (* cols 2)) brd))
             (and (>= move (+ 1 cols))
                  (<= move (- (* cols rows) cols))
                  (member? (- move cols) brd)
                  (member? (+ move cols) brd))
             (and (member? move lst1)
                  (member? (- move 1) brd)
                  (member? (- move 2) brd))
             (and (member? move lst2)
                  (member? (+ move 1) brd)
                  (member? (+ move 2) brd))
             (and (member? move lst3)
                  (member? (- move 1) brd)
                  (member? (+ move 1) brd))
             (and (member? move lst1)
                  (>= move (+ (* 2 cols) 1))
                  (member? (- move (+ cols 1)) brd)
                  (member? (- move (+ (* cols 2) 2)) brd))
             (and (member? move lst2)
                  (>= move (+ (* 2 cols) 1))
                  (member? (- move (- cols 1)) brd)
                  (member? (- move (- (* cols 2) 2)) brd))
             (and (member? move lst1)
                  (<= move (- (* cols rows) (* cols 2)))
                  (member? (+ move (- cols 1)) brd)
                  (member? (+ move (- (* cols 2) 2)) brd))
             (and (member? move lst2)
                  (<= move (- (* cols rows) (* cols 2)))
                  (member? (+ move (+ cols 1)) brd)
                  (member? (+ move (+ (* cols 2) 2)) brd))
             (and (member? move lst3)
                  (>= move (+ 1 cols))
                  (<= move (- (* cols rows) cols))
                  (member? (- move (+ cols 1)) brd)
                  (member? (+ move (+ cols 1)) brd))
             (and (member? move lst3)
                  (>= move (+ 1 cols))
                  (<= move (- (* cols rows) cols))
                  (member? (- move (- cols 1)) brd)
                  (member? (+ move (- cols 1)) brd))) #t]
        [else #f]))

;;Returns a list with possible win movements
(define (win-poss move brd cols rows)
  (define lst1 (check-right '3 '1 '3 cols rows '()))
  (define lst2 (check-left '1 '1 '1 cols rows '()))
  (define lst3 (check-middle '2 '1 '2 cols rows '()))
  (cond [(and (>= move (+ (* 2 cols) 1))
              (member? (- move cols) brd)
              (member? (- move (* cols 2)) brd))
         (list (- move cols) (- move (* cols 2)))]
        [(and (<= move (- (* cols rows) (* cols 2)))
              (member? (+ move cols) brd)
              (member? (+ move (* cols 2)) brd))
         (list (+ move cols) (+ move (* cols 2)))]
        [(and (>= move (+ 1 cols))
              (<= move (- (* cols rows) cols))
              (member? (- move cols) brd)
              (member? (+ move cols) brd))
         (list (- move cols) (+ move cols))]
        [(and (member? move lst1)
              (member? (- move 1) brd)
              (member? (- move 2) brd))
         (list (- move 1) (- move 2))]
        [(and (member? move lst2)
              (member? (+ move 1) brd)
              (member? (+ move 2) brd))
         (list (+ move 1) (+ move 2))]
        [(and (member? move lst3)
              (member? (- move 1) brd)
              (member? (+ move 1) brd))
         (list (- move 1) (+ move 1))]
        [(and (member? move lst1)
              (>= move (+ (* 2 cols) 1))
              (member? (- move (+ cols 1)) brd)
              (member? (- move (+ (* cols 2) 2)) brd))
         (list (- move (+ cols 1)) (- move (+ (* cols 2) 2)))]
        [(and (member? move lst2)
              (>= move (+ (* 2 cols) 1))
              (member? (- move (- cols 1)) brd)
              (member? (- move (- (* cols 2) 2)) brd))
         (list (- move (- cols 1)) (- move (- (* cols 2) 2)))]
        [(and (member? move lst1)
              (<= move (- (* cols rows) (* cols 2)))
              (member? (+ move (- cols 1)) brd)
              (member? (+ move (- (* cols 2) 2)) brd))
         (list (+ move (- cols 1)) (+ move (- (* cols 2) 2)))]
        [(and (member? move lst2)
              (<= move (- (* cols rows) (* cols 2)))
              (member? (+ move (+ cols 1)) brd)
              (member? (+ move (+ (* cols 2) 2)) brd))
         (list (+ move (+ cols 1)) (+ move (+ (* cols 2) 2)))]
        [(and (member? move lst3)
              (>= move (+ 1 cols))
              (<= move (- (* cols rows) cols))
              (member? (- move (+ cols 1)) brd)
              (member? (+ move (+ cols 1)) brd))
         (list (- move (+ cols 1)) (+ move (+ cols 1)))]
        [(and (member? move lst3)
              (>= move (+ 1 cols))
              (<= move (- (* cols rows) cols))
              (member? (- move (- cols 1)) brd)
              (member? (+ move (- cols 1)) brd))
         (list (- move (- cols 1)) (+ move (- cols 1)))]

        [else '()]))
  
;;Check if item is in given list
(define (member? x lst) (not (false? (member x lst)))) 

;;Checks if its player's turn, returns true if it is, false otherwise
(define (player-turn? turn)
  (cond [(equal? turn 'p) #t]
        [else #f]))

;;Checks thru all states of winning if there was a winning move
(define (win-move? move brd cols rows)
  (cond [(win-state? move brd cols rows) #t]
        [else #f]))


;;Adds the move done to the corresponding list, player's list or computer's list
(define (register-move move brd)
  (cond [(and (player-turn? (car brd))
              (member? move (cadddr brd)))
         (list 'c
            (cadr brd)
            (append (caddr brd) (list move))
            (remove move (cadddr brd)))]
        [(and (not(player-turn? (car brd)))
              (member? move (cadddr brd)))
         (list 'p
            (append (cadr brd) (list move))
            (caddr brd)
            (remove move (cadddr brd)))]))
      

;;Produces a list of boards one move into the future
;;(define (poss-brds board)
  ;;(local[(define (each-rem board rem-lst)
    ;;       (cond[(empty? rem-lst) '()]
      ;;          [else (append (list (register-move (first rem-lst) board))
        ;;                      (each-rem board (rest rem-lst)))]))]
    ;;(each-rem board (fourth board))))

;;
;;Produces list of all possible movements
(define (candidates brd)
  (cond [(null? (cadddr brd)) #f]
        [else (cadddr brd)]))

;;Checks if candidate is suitable
(define (viability brd cols rows move)
  (cond [(equal? (list-length (caddr brd)) 1) (register-move move brd)]
        [else (register-move move brd)]))

;;Selects best candidate
(define (selection brd candidates cols rows)
  (cond [(equal? (list-length (caddr brd)) 1)
         (cond [(equal? (caaddr brd) 1) (viability brd cols rows (+ (caaddr brd) 1))]
               [(equal? (caaddr brd) cols) (viability brd cols rows (- (caaddr brd) 1))]
               [(equal? (caaddr brd) (- (* cols rows) (- cols 1))) (viability brd cols rows (+ (caaddr brd) 1))]
               [(equal? (caaddr brd) (* cols rows)) (viability brd cols rows (- (caaddr brd) 1))]
               [(and (or (<= (caaddr brd) cols)
                         (>= (caaddr brd) (- (* cols rows) (- cols 1))))
                     (member? (caaddr brd) (check-middle '2 '1 '2 cols rows '())))
                (viability brd cols rows (- (caaddr brd) 1))]
               [(and (> (caaddr brd) cols)
                     (<= (caaddr brd) (- (* cols rows) cols))
                     (or (member? (caaddr brd) (get-left '1 '1 '1 cols rows '()))
                         (member? (caaddr brd) (get-right cols '1 cols cols rows '()))))
                (viability brd cols rows (- (caaddr brd) cols))]
               [else (viability brd cols rows (- (caaddr brd) (- cols 1)))])]
        [(and (>= (list-length (caddr brd)) 2)
              (not(null? (poss-win (caddr brd) (cadddr brd) cols rows))))
         (viability brd cols rows (poss-win (caddr brd) (cadddr brd) cols rows))]
        [(and (>= (list-length (caddr brd)) 2)
              (null? (poss-win (caddr brd) (cadddr brd) cols rows))
              (not (null? (poss-win (cadr brd) (cadddr brd) cols rows))))
         (viability brd cols rows (poss-win (cadr brd) (cadddr brd) cols rows))]
        [(and (>= (list-length (caddr brd)) 2)
              (null? (poss-win (caddr brd) (cadddr brd) cols rows))
              (null? (poss-win (cadr brd) (cadddr brd) cols rows)))
         (viability brd cols rows (best-choice (cadr brd) (cadddr brd) '() cols rows))]
        [else #f]))

(define (poss-win movesP movesA cols rows)
  (cond [(null? movesA) movesA]
        [(win-move? (car movesA) (car (list movesP)) cols rows) (car movesA)]
        [else (poss-win movesP (cdr movesA) cols rows)]))

(define (best-choice movesC movesA movesA2 cols rows)
  (define lst1 (append movesA movesA2))
  (cond [(null? movesC) (car movesA)]
        [(null? movesA) (best-choice (cdr movesC) lst1 '() cols rows)]
        [(not (null? (win-poss (car movesC) lst1 cols rows))) (car (win-poss (car movesC) lst1 cols rows))]
        [else (best-choice movesC (cdr movesA) (append movesA2 (list (car movesA))) cols rows)]))

(define (best-choice-aux adMoves movesA)
  (cond [(null? adMoves) adMoves]
        [(null? movesA) movesA]
        [(member? (car adMoves) movesA) (car adMoves)]
        [else (best-choice-aux (cdr adMoves) movesA)]))
        
        

;; see interface above [no further info required]
(define (depth lst n)
  (cond [(number? lst) (list (- lst n))]
        [(empty? lst) '()]
        [else (append (depth (first lst) (+ n .1))
                      (depth (rest lst) n))]))