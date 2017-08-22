#lang racket

;;; File:
;;;   square.rkt
;;; Summary:
;;;   A sample function for Racket, intended as an example for
;;;   students (or for myself..
;;; Author:
;;;   Samuel A. Rebelsky

(provide
  (contract-out
    (square (-> number? number?))))

; +---------------------+--------------------------------------------
; | Exported procedures |
; +---------------------+

;;; Procedure:
;;;   square
;;; Parameters:
;;;   num, a number
;;; Purpose:
;;;   Compute the square of num
;;; Produces:
;;;   squared, a number
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (sqrt squared) is approximately num
;;;   squared has the same "type" as num
;;;     If num is an integer, squared is an integer
;;;     If num is real, squared is real
;;;     If num is exact, squared is exact
;;;     If num is inexact, squared is inexact
;;;     And so on and so forth
(define square
  (lambda (num)
    (* num num)))

