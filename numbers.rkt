#lang racket

;;; File:
;;;   numbers.rkt
;;; Summary:
;;;   A variety of procedure associated with numbers.
;;; Author:
;;;   Samuel A. Rebelsky

(provide
  (contract-out
    [decrement (-> number? number?)]
    [increment (-> number? number?)]
    ))

; +---------------------+--------------------------------------------
; | Exported procedures |
; +---------------------+

;;; Package:
;;;   csc151/numbers
;;; Procedure:
;;;   decrement
;;; Parameters:
;;;   val, a number
;;; Purpose:
;;;   Subtract 1 from val
;;; Produces:
;;;   decrement, a number
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   decremented = (- val 1)
;;; Ponderings:
;;;   An obvious procedure, but one that is often useful.
(define decrement
  (lambda (val)
    (- val 1)))

;;; Package:
;;;   csc151/numbers
;;; Procedure:
;;;   increment
;;; Parameters:
;;;   val, a number
;;; Purpose:
;;;   Add 1 to val
;;; Produces:
;;;   incremented, a number
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   incremented = (+ val 1)
;;; Ponderings:
;;;   An obvious procedure, but one that is often useful.
(define increment 
  (lambda (val)
    (+ val 1)))

