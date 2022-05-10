#lang racket

(provide player-turn? win-move? register-move candidates selection viability poss-win best-choice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IMPLEMENTATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A board is a (list turn (list computer's moves) (list player's moves) (list available moves))
;; Given a board and a movement the win algorithm checks all possible win combinations and returns true or false
;; Given a board the computer's algorithm checks all possible win movements and blocks the player win possibilities and goes for win combinations too


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
  

;;Returns list of all right available spaces in the grid
(define (check-right contador1 contador2 contador3 cols rows lst)
  (cond [(equal? contador2 (+ rows 1)) lst]
        [(equal? contador1 cols) (check-right 3 (+ contador2 1) (+ contador3 3) cols rows (append lst (list contador3)))]
        [else (check-right (+ contador1 1) contador2 (+ contador3 1) cols rows (append lst (list contador3)))]))

;;Returns list of all left available spaces in the grid
(define (check-left contador1 contador2 contador3 cols rows lst)
  (cond [(equal? contador2 (+ rows 1)) lst]
        [(equal? contador1 (- cols 2)) (check-left 1 (+ contador2 1) (+ contador3 3) cols rows (append lst (list contador3)))]
        [else (check-left (+ contador1 1) contador2 (+ contador3 1) cols rows (append lst (list contador3)))]))

;;Returns list of all middle available spaces in the grid
(define (check-middle contador1 contador2 contador3 cols rows lst)
  (cond [(equal? contador2 (+ rows 1)) lst]
        [(equal? contador1 (- cols 1)) (check-middle 2 (+ contador2 1) (+ contador3 3) cols rows (append lst (list contador3)))]
        [else (check-middle (+ contador1 1) contador2 (+ contador3 1) cols rows (append lst (list contador3)))]))

;;Returns list of left column of the grid
(define (get-left contador1 contador2 contador3 cols rows lst)
  (cond [(equal? contador2 (+ rows 1)) lst]
        [(equal? contador1 1) (get-left 1 (+ contador2 1) (+ contador3 cols) cols rows (append lst (list contador3)))]))

;;Returns list of right column of the grid
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

;;Checks all above win-states for memory usage reduction, returns true if win combination was found, false otherwise
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

;;Returns a list with possible win movements, uses same algorithm as above
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


;;Adds the move done to the corresponding list, player's list or computer's list, !!!!!!and checks if the movement added to add is a win
(define (register-move move brd cols rows)
  (cond [(and (player-turn? (car brd))
              (member? move (cadddr brd)))
         (cond [(win-move? move brd cols rows)
                (list 'c
                (cadr brd)
                (append (caddr brd) (list move))
                (remove move (cadddr brd))
                (fifth #t))]
               [else (list 'c
                (cadr brd)
                (append (caddr brd) (list move))
                (remove move (cadddr brd))
                (fifth #f))])]
        [(and (not(player-turn? (car brd)))
              (member? move (cadddr brd)))
         (cond [(win-move? move brd cols rows)
                (list 'p
                (append (cadr brd) (list move))
                (caddr brd)
                (remove move (cadddr brd))
                (fifth #t))]
               [else (list 'p
                (append (cadr brd) (list move))
                (caddr brd)
                (remove move (cadddr brd))
                (fifth #f))])]))
      

;;Produces list of all possible movements, if there's no available movements, returns an empty list
(define (candidates brd)
  (cond [(null? (cadddr brd)) '()]
        [else (cadddr brd)]))

;;Selects best candidate
(define (selection brd candidates cols rows)
  (cond [(equal? (list-length (caddr brd)) 1) ;;If there's only one player move
         (cond [(equal? (caaddr brd) 1) (viability brd cols rows (+ (caaddr brd) 1))] ;;If the player's move is on any corner checks which move is better
               [(equal? (caaddr brd) cols) (viability brd cols rows (- (caaddr brd) 1))]
               [(equal? (caaddr brd) (- (* cols rows) (- cols 1))) (viability brd cols rows (+ (caaddr brd) 1))]
               [(equal? (caaddr brd) (* cols rows)) (viability brd cols rows (- (caaddr brd) 1))]
               [(and (or (<= (caaddr brd) cols) ;;If player's move is on top or bottom rows check which move is better
                         (>= (caaddr brd) (- (* cols rows) (- cols 1))))
                     (member? (caaddr brd) (check-middle '2 '1 '2 cols rows '())))
                (viability brd cols rows (- (caaddr brd) 1))]
               [(and (> (caaddr brd) cols) ;;If player's move is on left or right columns check which move is better
                     (<= (caaddr brd) (- (* cols rows) cols))
                     (or (member? (caaddr brd) (get-left '1 '1 '1 cols rows '()))
                         (member? (caaddr brd) (get-right cols '1 cols cols rows '()))))
                (viability brd cols rows (- (caaddr brd) cols))]
               [else (viability brd cols rows (- (caaddr brd) (- cols 1)))])]
        [(and (>= (list-length (caddr brd)) 2) ;;Else if there's two or more player moves and there's a win chance for the player, block it
              (not(null? (poss-win (caddr brd) (cadddr brd) cols rows))))
         (viability brd cols rows (poss-win (caddr brd) (cadddr brd) cols rows))]
        [(and (>= (list-length (caddr brd)) 2) ;;Else if there's two or more player moves and there's no win chance for the player and there's a win chance for the computer, take the fittest win combination
              (null? (poss-win (caddr brd) (cadddr brd) cols rows))
              (not (null? (poss-win (cadr brd) (cadddr brd) cols rows))))
         (viability brd cols rows (poss-win (cadr brd) (cadddr brd) cols rows))]
        [(and (>= (list-length (caddr brd)) 2) ;;Else if there's two or more player moves and there's no win chance for player and computer just choose any move randomly
              (null? (poss-win (caddr brd) (cadddr brd) cols rows))
              (null? (poss-win (cadr brd) (cadddr brd) cols rows)))
         (viability brd cols rows (best-choice (cadr brd) (cadddr brd) '() cols rows))]
        [else #f]))

;;Checks if candidate is viable and if it's then it registers the move into the playing board
(define (viability brd cols rows move)
  (cond [(equal? (list-length (caddr brd)) 1) (register-move move brd cols rows)]
        [else (register-move move brd cols rows)]))

;;Given a set of movements, returns the movement that would produce a win, if there's non returns an empty list
(define (poss-win movesP movesA cols rows)
  (cond [(null? movesA) movesA]
        [(win-move? (car movesA) (car (list movesP)) cols rows) (car movesA)]
        [else (poss-win movesP (cdr movesA) cols rows)]))

;;Given a set of movements, returns the best possible movement in order to win, if there's non returns an empty list
(define (best-choice movesC movesA movesA2 cols rows)
  (define lst1 (append movesA movesA2))
  (cond [(null? movesC) (car movesA)]
        [(null? movesA) (best-choice (cdr movesC) lst1 '() cols rows)]
        [(not (null? (win-poss (car movesC) lst1 cols rows))) (car (win-poss (car movesC) lst1 cols rows))]
        [else (best-choice movesC (cdr movesA) (append movesA2 (list (car movesA))) cols rows)]))