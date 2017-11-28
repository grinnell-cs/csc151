#lang racket

;;; File:
;;;   counters.rkt
;;; Summary:
;;;   Simple counters for CSC 151.
;;; Author:
;;;   Samuel A. Rebelsky

(provide (all-defined-out))

; +---------------------+--------------------------------------------
; | Exported procedures |
; +---------------------+

;;; Procedure:
;;;   counter-new
;;; Parameters:
;;;   name, a string
;;; Purpose:
;;;   Create a counter associated with the given name.
;;; Produces:
;;;   counter, a counter
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   counter can be used as a parameter to the various counter
;;;   procedures.
;;; Process:
;;;   Counters are two element vectors.  Element 0 is the name, and
;;;   should not change.  Element 1 is the count, and should change.
(define counter-new
  (lambda (name)
    (vector name 0)))

;;; Procedure:
;;;   counter-increment!
;;; Parameters:
;;;   counter, a counter 
;;; Purpose:
;;;   count the counter
;;; Produces:
;;;   counter, the same counter, now mutated
;;; Preconditions:
;;;   counter was created by counter-new (or something similar) and
;;;   has only been modified by the counter procedures.
;;; Postconditions:
;;;   (counter-get counter) gives a number one higher than it 
;;;   did before.
(define counter-increment!
  (lambda (counter)
    (vector-set! counter 1 (+ 1 (vector-ref counter 1)))
    counter))

;;; Procedure:
;;;   counter-get
;;; Parameters:
;;;   counter, a counter
;;; Purpose:
;;;   Get the number of times that counter-count has been called
;;;   on this counter.
;;; Produces:
;;;   count, a non-negative integer
;;; Preconditions:
;;;   counter was created by counter-new and has only been modified
;;;   by the counter procedures.
;;; Postconditions:
;;;   count is the number of calls to counter-new on this counter since
;;;   the last call to counter-reset! on this counter, or since the
;;;   counter was created, if there have been no calls to counter-reset!
(define counter-get
  (lambda (counter)
    (vector-ref counter 1)))

;;; Procedure:
;;;   counter-reset!
;;; Parameters:
;;;   counter, a counter 
;;; Purpose:
;;;   reset the counter
;;; Produces:
;;;   counter, the same counter, now set to 0
;;; Preconditions:
;;;   counter was created by counter-new (or something similar) and
;;;   has only been modified by the other counter procedures.
;;; Postconditions:
;;;   (counter-get counter) gives 0.
(define counter-reset!
  (lambda (counter)
    (vector-set! counter 1 0)
    counter))

;;; Procedure:
;;;   counter-print
;;; Parameters:
;;;   counter, a counter
;;; Purpose:
;;;   Print out the information associated with the counter.
;;; Produces:
;;;   counter, the same counter
;;; Preconditions:
;;;   counter was created by counter-new and has only been modified
;;;   by the various counter procedures.
;;; Postconditions:
;;;   counter is unchanged.
;;;   The output port now contains information on counter.
;;; Ponderings:
;;;   Why does counter-print have a bang, given that it doesn't mutate
;;;   it's parameter?  Because it mutates the broader environment - we
;;;   call counter-print not to compute a value, but to print something.
(define counter-print
  (lambda (counter)
    (display (vector-ref counter 0))
    (display ": ")
    (display (vector-ref counter 1))
    (newline)))

