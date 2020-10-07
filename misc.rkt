#lang racket

;;; File:
;;;   misc.rkt
;;; Summary:
;;;   A variety of useful procedures.
;;; Author:
;;;   Samuel A. Rebelsky
;;;   Peter-Michael Osera

(provide
  (contract-out
    [log-addition (-> number? number? number?)]
    [log-value (-> any/c any)]
    [member? (-> any/c (listof any/c) boolean?)]
    [writeln (-> any/c any)]
    [show-call (->* (symbol?) () #:rest (listof any/c) any)]
    [nat? (-> any/c boolean?)]))

(provide ??)

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
;;;   member?
;;; Parameters:
;;;   val, a Scheme value
;;;   lst, a list
;;; Purpose:
;;;   Determine if `val` appears in `lst.
;;; Produces:
;;;   in-list?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   * If there exists an i s.t. (equal? val (list-ref lst i)), then
;;;     in-list? is true (#t)
;;;   * Otherwise, in-list? is false (#f)
(define member?
   (lambda (val lst)
     (and (not (null? lst))
          (or (equal? val (car lst))
              (member? val (cdr lst))))))

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

(define nat? exact-nonnegative-integer?)

;;; Package:
;;;   csc151/misc
;;; Procedure:
;;;   ??
;;; Paramters:
;;;   [Nothing]
;;; Purpose:
;;;   Placeholder value for an expression.  Should be used as a zero-
;;;   argument function call, e.g., (??), in parts of code that
;;;   should be filled in at a later date.  Raises an error when
;;;   evaluated.
;;; Preconditions:
;;;   [None]
;;; Postconditions:
;;;   [None]
(define ??
  (lambda () (error "Hole encountered!  Fill me in!")))
