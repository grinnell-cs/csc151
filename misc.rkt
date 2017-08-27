#lang racket

;;; File:
;;;   misc.rkt
;;; Summary:
;;;   A variety of useful procedures.
;;; Author:
;;;   Samuel A. Rebelsky

(provide
  (contract-out
    (writeln (-> any/c any))))

; +---------------------+--------------------------------------------
; | Exported procedures |
; +---------------------+

;;; Package:
;;;   csc151/misc
;;; Procedure:
;;;   writeln
;;; Parameters:
;;;   val, a scheme value
;;; Purpose:
;;;   Writes val followed by a newline
;;; Produces:
;;;   [Nothing; called for the side effects.]
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   A representation of val appears on the output, followed by 
;;;   a newline.
(define writeln
  (lambda (val)
    (write val)
    (newline)))

