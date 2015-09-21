;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mergesort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Austin Luo (20558943)
;; CS 135 Fall 2014
;; Assignment 10, Problem 1 (mergesort)
;; Instructor: Curtis Bright
;; **************************

;;(mergesort list f) consumes a (list) and a comparator function (f)
;; and outputs the list in a sorted order based on the comparator
;; by splitting the list into sublists and merging the sublists together
;;mergesort: (listOf X) (X X -> Bool) -> (listOf X)
;;Examples:
(check-expect (mergesort '(1 4 6 2 4) <) '(1 2 4 4 6))
(check-expect (mergesort (list "apple" "dear" "brother" "complex") string<?)
              (list "apple" "brother" "complex" "dear")) 

(define (mergesort lst f)
  (cond
    [(empty? lst) empty]
    [(empty? (rest lst)) lst]
    [else (local [(define first-half
                    (foldr (lambda (x y) 
                             (cond [(> (ceiling (/ (length lst) 2)) 
                                       (length y))
                                    (cons x y)]
                                   [else y])) empty lst))
                  (define second-half
                    (foldl (lambda (x y)
                             (cond [(> (floor (/ (length lst) 2))
                                       (length y))
                                    (cons x y)]
                                   [else y])) empty lst))
                  ;;(merge lst1 lst2) consumes two sorted lists (lst1, lst2)
                  ;; and outputs a combined list in sorted order
                  (define (merge lst1 lst2)
                    (cond
                      [(empty? lst1) lst2]
                      [(empty? lst2) lst1]
                      [(f (first lst1) (first lst2))
                       (cons (first lst1) (merge (rest lst1) lst2))]
                      [else (cons (first lst2) (merge lst1 (rest lst2)))]))]
            (merge (mergesort first-half f) (mergesort second-half f)))]))

;;Tests
(check-expect (mergesort empty <) empty)
(check-expect (mergesort (list 1 2 1) <=) (list 1 1 2))
(check-expect (mergesort (list 1 2 3 4 5) >=) (list 5 4 3 2 1))
(check-expect (mergesort (list 1 2 3 4 5) <) (list 1 2 3 4 5))
(check-expect (mergesort (list 4 7 3 3 7 9 4) <) (list 3 3 4 4 7 7 9))
(check-expect (mergesort (list -1 8 4 5 -2 -9 0) >) (list 8 5 4 0 -1 -2 -9))