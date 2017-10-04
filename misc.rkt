#lang racket

;;; File:
;;;   misc.rkt
;;; Summary:
;;;   A variety of useful procedures.
;;; Author:
;;;   Samuel A. Rebelsky

(provide
  (contract-out
    [log-addition (-> number? number? number?)]
    [log-value (-> any/c any)]
    [writeln (-> any/c any)]))

; +---------------------+--------------------------------------------
; | Exported procedures |
; +---------------------+

;;; Procedure:
;;;   csc151/misc
;;; Procedure:
;;;   log-addition
;;; Parameters:
;;;   a, a number
;;;   b, a number
;;; Purpose:
;;;   Add two numbers and log a note that we did the addition
;;; Produces:
;;;   sum, a number
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   sum = (+ a b)
;;;   A report of the addition has been logged to standard output.
(define log-addition
  (lambda (a b)
    (let ([result (+ a b)])
      (display a)
      (display " + ")
      (display b)
      (display " = ")
      (display result)
      (newline)
      result)))

;;; Package:
;;;   csc151/misc
;;; Procedure:
;;;   log-value
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Writes val followed by a newline
;;; Produces:
;;;   val, the same Scheme value
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   A representation of val appears on the output, followed by 
;;;   a newline.
;;; Philosophy:
;;;   Useful when we want to track what is happening in our program.
(define log-value
  (lambda (val)
    (write val)
    (newline)
    val))

;;; Package:
;;;   csc151/misc
;;; Procedure:
;;;   writeln
;;; Parameters:
;;;   val, a Scheme value
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

;;; Package:
;;;   csc151/misc
;;; Procedure:
;;;   show-call
;;; Parameters:
;;;   proc, a symbol
;;;   val1 ... valn, a bunch of Scheme values
;;; Purpose:
;;;   Shows a procedure call
;;; Produces:
;;;   [Nothing; called for the side effects.]
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   A representation of the procedure call appears on the output,
;;;   followed by a newline.
(define show-call
  (lambda (proc . params)
    (display "(")
    (display proc)
    (for-each (lambda (param) (display " ") (write param))
              params)
    (display ")")
    (newline)))
