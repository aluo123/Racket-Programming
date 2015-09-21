;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname solitaire) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; The following line is REQUIRED (do not remove)
(require "a10lib.rkt")

;; Austin Luo (20558943)
;; CS 135 Fall 2014
;; Assignment 9, Problem 2 (solitaire)
;; Instructor: Curtis Bright
;; **************************

;; NOTE: Do NOT leave top-level expressions in your code.
;;       In other words, when your code is run, only the
;;       check-expect message "All X tests passed!"
;;       should appear in the interactions window

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Dimension is an Int
;; requires: 1 <= Dimension <= 9

;; A Peg [position] is an Int
;; requires: 11 <= Peg <= 99
;;           neither digit can be zero or greater than the
;;             Dimension (for the corresponding board)

;; A Board is a (list Dimension (listof Peg))
;; The list contains INVALID Peg positions

;; A State is a (listof Peg)
;; requires: list is non-emtpy
;;           each Peg is VALID for the corresponding board

;; A Solution is one of:
;; * 'any
;; * Peg


(define no-solution-text (list (list "No Solution Found")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the sample board from the assignment

(define sample (list 4 (list 41 42 43 44)))
#|
....
....
....
    
|#

(define sample/init (list 22 23))
#|
....
.OO.
....
    
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the traditional cross pattern with default init state cross/init
;; with some additional (easier) init states you can use

(define cross (list 7 (list 11 12 16 17 21 22 26 27 61 62 66 67 71 72 76 77)))
#|
  ...  
  ...  
.......
.......
.......
  ...  
  ...  
|#

(define cross/init (list 13 14 15 23 24 25 31 32 33 34 35 36 37 41 42 43
                         45 46 47 51 52 53 54 55 56 57 63 64 65 73 74 75))
#|
  OOO  
  OOO  
OOOOOOO
OOO.OOO
OOOOOOO
  OOO  
  OOO  
|#

(define cross/submarine (list 34 42 43 44 45 46))
#|
  ...  
  ...  
...O...
.OOOOO.
.......
  ...  
  ...  
|#

(define cross/greek (list 24 34 42 43 44 45 46 54 64))
#|
  ...  
  .O.  
...O...
.OOOOO.
...O...
  .O.  
  ...  
|#

(define cross/small-diamond (list 24 33 34 35 42 43 45 46 53 54 55 64))
#|
  ...  
  .O.  
..OOO..
.OO.OO.
..OOO..
  .O.  
  ...  
|#

(define cross/big-diamond (list 14 23 24 25 32 33 34 35 36 41 42 43
                                45 46 47 52 53 54 55 56 63 64 65 74))
#|
  .O.  
  OOO  
.OOOOO.
OOO.OOO
.OOOOO.
  OOO  
  .O.  
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;(build-board dim) consumes a Dimension (dim) and outputs a list
;; of rows filled with pegs corresponding the Dimension
;;build-board: Dimension -> (listOf (listOf Peg))
;;Examples
(check-expect (build-board 3) 
              (list (list 11 12 13) (list 21 22 23) (list 31 32 33)))
(check-expect (build-board 1)
              (list (list 11)))

(define (build-board dim)
  (build-list dim 
              (lambda (x) 
                (build-list dim 
                            (lambda (y) 
                              (+ (* 10 (add1 x)) (add1 y)))))))

;;Tests
(check-expect (build-board 2)
              (list (list 11 12) (list 21 22)))
(check-expect (build-board 4)
              (list (list 11 12 13 14)
                    (list 21 22 23 24)
                    (list 31 32 33 34)
                    (list 41 42 43 44)))

;;(state->los board state) consumes a (board) and a (state)
;; and produces a list of strings to visualize the pegs
;;state->los: Board State -> (listOf Str)
;;Example
(check-expect (state->los sample sample/init)
              (list "...." ".OO." "...." "    "))
(check-expect (state->los cross cross/init)
              (list "  OOO  "
                    "  OOO  "
                    "OOOOOOO"
                    "OOO.OOO"
                    "OOOOOOO"
                    "  OOO  "
                    "  OOO  "))

(define (state->los board state)
  (map (lambda (y) 
         (list->string (map (lambda (x) 
                              (cond [(member? x state) #\O]
                                    [(member? x (second board)) #\space]
                                    [else #\.]))
                            y))) (build-board (first board))))

;;Tests
(check-expect (state->los (list 1 empty) empty)
              (list "."))
(check-expect (state->los (list 1 empty) (list 11))
              (list "O"))

;;(make-solved? sol) consumes a solution (sol) and outputs a 
;; predicate that consumes a state that outputs whether or
;; the state is a solution
;;make-solved?: Solution -> (State -> Bool)
;;Examples:
(check-expect ((make-solved? 'any) (list 44 45)) false)
(check-expect ((make-solved? 45) (list 45)) true)

(define (make-solved? sol)
  (lambda (x) (and (= (length x) 1)
                   (or (equal? sol 'any)
                       (= sol (first x))))))

;;Tests
(check-expect ((make-solved? 'any) (list 44)) true)
(check-expect ((make-solved? 45) (list 44)) false)
(check-expect ((make-solved? 'any) (list 11)) true)
(check-expect ((make-solved? 45) (list 44 45)) false)

;;(neighbours board state) consumes a (board) and a (state)
;; and outputs all states that are legal moves from the 
;; consumes state
;;neighbours: Board State -> (listOf State)
;;Examples:
(check-expect (neighbours (list 4 (list 41 42 43 44))
                          (list 22 23))
              (list (list 24) (list 21)))
(check-expect (neighbours (list 4 (list 41 42 43 44))
                          (list 11 12 21 22))
              (list (list 13 21 22)
                    (list 12 22 31)
                    (list 11 21 32)
                    (list 11 12 23)))

(define (neighbours board state)  
  (foldr (lambda (a b)(append a b)) 
         empty
         (map 
          (lambda (x)
            (local 
              [;;(valid-peg? peg) consumes a peg a sees if it is a 
               ;; valid position on the board
               ;;valid-peg?: Peg -> Bool
               (define (valid-peg? peg)
                 (and (member? true (map (lambda (y) (member? peg y)) 
                                         (build-board (first board))))
                      (not (member? peg (second board)))))
               (define right (list (+ x 1) (+ x 2)))
               (define left (list (- x 1) (- x 2)))
               (define up (list (- x 10) (- x 20)))
               (define down (list (+ x 10) (+ x 20)))
               (define lod (list right left up down))
               (define (valid-move? dir)
                 (and (valid-peg? (first dir))
                      (valid-peg? (second dir))
                               (member? (first dir) state)
                               (not (member? (second dir) state))))
               ;;(insert elmnt lst pred?) consumes an element (elmnt),
               ;; a sorted list (lst) and a predicate (pred?) and outputs
               ;; a new sorted list with that element
               ;;insert: X (listOf X) (X -> Bool) -> (listOf X)
               (define (insert elmnt lst pred?)
                 (cond
                   [(empty? lst) (cons elmnt empty)]
                   [(pred? elmnt (first lst))
                    (cons elmnt lst)]
                   [else (cons (first lst) (insert elmnt (rest lst) pred?))]))
               ;;(moves-peg lod) consumes a list of directions (lod)
               ;; and outputs all valid states that peg can move
               ;;moves-peg: Peg -> (listOf State)
               (define (moves-peg lod)
                 (cond 
                   [(empty? lod) empty]
                   [(valid-move? (first lod))
                    (cons (insert (second (first lod))
                                  (remove (first (first lod))
                                          (remove x state)) <)
                          (moves-peg (rest lod)))]
                   [else (moves-peg (rest lod))]))]
              (moves-peg lod))) state)))

;;Tests
(check-expect (neighbours (list 1 empty) (list 11)) empty)
(check-expect (neighbours (list 4 (list 41 42 43 44))
                          (list 21 22 32 33))
              (list (list 23 32 33)
                    (list 21 22 34)
                    (list 12 21 33)
                    (list 21 22 31)))

;; This is a provided function: no additional documentation required
;; Uncomment the following line after your neighbours is complete and tested
(define (make-neighbours board) (lambda (state) (neighbours board state)))

;;(solitaire board state sol) consumes a (board), a (state)
;; and a solution (sol) and produces a list of the states
;; to that solution, or false if the solution doesn't exist
;;solitaire: Board State Solution -> (anyOf (listOf State) false))
;;Examples:

(define (solitaire board state sol)
  (find-route state (make-neighbours board) (make-solved? sol)))

;;(result->text board result) consumes a (board) and a result from
;; the solitaire function and outputs a text visual representation 
;; of each state up to the solution
;;result->text: Board (listOf (listOf State)) -> (listOf (listOf Str))

(define (result->text board result)
  (cond [(false? result) no-solution-text]
        [else (map (lambda (state) (state->los board state)) result)]))

;; try this when you are done: (but leave it commented out when you submit)
; (show (result->text cross (solitaire cross cross/init 'any)))
