#lang racket

;;; File:
;;;   verbose-bindings.rkt
;;; Author:
;;;   Samuel A. Rebelsky
;;; Summary:
;;;   A set of macros that make it easier to observe what's
;;;   happening when we bind names to values.
;;; Contents:
;;;   (verbose-define name value)
;;;      Assign value to name, describing the binding.
;;;   (verbose-let (name-value-pairs) exp1 ...)
;;;      Assign each value to the corresponding name, but
;;;      also describes the bindings.
;;;   (verbose-let* (name-value-pairs) exp1 ...)
;;;      Assign each value to the corresponding name, in sequence.
;;; Citations:
;;;   Based closely on code written by Samuel A. Rebelsky in about
;;;   2006.

(provide verbose-define)
(provide verbose-let)
(provide verbose-let*)

; +--------+---------------------------------------------------------------
; | Macros |
; +--------+

;;; Procedure (Macro):
;;;   verbose-define
;;; Parameters:
;;;   name, a symbol [unverified]
;;;   exp, a valid expression.
;;; Purpose:
;;;   Globally binds val to name while reporting the binding (for
;;;   observation).
;;; Preconditions:
;;;   val must be a valid expression.
;;; Postconditions:
;;;   name can be now used in future expressions, and gives the value
;;;     of exp.
;;; Philosophy:
;;;   It may be helpful to students learning about the techniques for
;;;   binding values to see the bindings happen.  This procedure does
;;;   little more than report the binding and then call define.
(define-syntax verbose-define
  (syntax-rules ()
    [(verbose-define name exp)
     (begin
       (report-binding name exp "define")
       (define name exp))]))

;;; Procedure (Macro):
;;;   verbose-let
;;; Parameters:
;;;   list-of-bindings, a list of name/expression lists
;;;   exp0 ..., a list of expressions
;;; Purpose:
;;;   Does the same thing as (let list-of-bindings exp0 ...), except that
;;;   it also provides a report on what happens.
;;; Preconditions:
;;;   The corresponding let operation must be valid.
;;; Postconditions:
;;;   See those for let.
;;; Philosophy:
;;;   It may be helpful to students learning about the techniques for
;;;   binding values to see the bindings happen.  This procedure does
;;;   little more than report the binding and then call let.
(define-syntax verbose-let
  (syntax-rules ()
    [(verbose-let definitions body ...)
     (begin
       (display "Beginning let.")
       (newline)
       (display-let-bindings definitions)
       (let ([result (let definitions body ...)])
         (display "Ending let.")
         (newline)
         result))]))

;;; Procedure (Macro):
;;;   verbose-let*
;;; Parameters:
;;;   list-of-bindings, a list of name/expression lists
;;;   exp0 ..., a list of expressions
;;; Purpose:
;;;   Does the same thing as (let* list-of-bindings exp0 ...), except that
;;;   it also provides a report on what happens.
;;; Preconditions:
;;;   The corresponding let* operation must be valid.
;;; Postconditions:
;;;   See those for let*.
;;; Philosophy:
;;;   It may be helpful to students learning about the techniques for
;;;   binding values to see the bindings happen.  This procedure shows them
;;;   as they happen.
;;; Props:
;;;   This code is based on a definition of let* taken from Chapter 8 of
;;;   the 3rd edition of R Kent Dybvig's _The Scheme Programming Language_,
;;;   found on the Web at <http://www.scheme.com/tspl3/syntax.html>.
(define-syntax verbose-let*
  (syntax-rules ()
    [(verbose-let* definitions body ...)
     (begin
       (display "Beginning let*")
       (newline)
       (let ((result (verbose-let*-helper definitions body ...)))
         (display "Ending let*")
         (newline)
         result))]))

(define-syntax verbose-let*-helper
  (syntax-rules ()
    [(verbose-let*-helper () e0 ...) 
     (let () e0 ...)]
    [(verbose-let*-helper ((n0 v0) (n1 v1) ...) e0 ...)
     (begin
       (report-binding n0 v0 "let*.")
       (let ((n0 v0)) 
         (verbose-let*-helper ((n1 v1) ...) e0 ...)))]))


; +---------+--------------------------------------------------------------
; | Helpers |
; +---------+

(define-syntax report-binding
  (syntax-rules ()
    ((_ name exp how)
     (begin
       (display "Binding ")
       (write 'name)
       (display " to ")
       (write 'exp)
       (display " with ")
       (display how)
       (newline)))))

(define-syntax display-let-bindings
  (syntax-rules ()
    ((_ ())
     (display ""))
    ((_ ((n1 v1) (n2 v2) ...))
     (begin
       (display "  ")
       (report-binding n1 v1 "let")
       (display-let-bindings ((n2 v2) ...))))))

