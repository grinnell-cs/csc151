#lang racket

;;; File:
;;;   cloneable.rkt
;;; Summary:
;;;   Tools for dealing with cloneable objects
;;; Author:
;;;   Samuel A. Rebelsky

(require racket/generic)
(require "sstruct.rkt")

(provide (all-defined-out))

; +---------------+--------------------------------------------------
; | The interface |
; +---------------+

;;; (cloneable? val) -> boolean?
;;;   val : any?
;;; Determine if val is cloneable. You can call `clone` on cloneable values.
(define-generics cloneable
  (clone cloneable)
  #:fallbacks
  [(define clone
     (lambda (val)
       (error 'clone "~a is not cloneable" val)))])

; +-----------------------+------------------------------------------
; | A deprecated approach |
; +-----------------------+

;;; (struct-cloneable name ...) -> (void?)
;;;   name : identifier?
;;; Make a cloneable structure.
;;;
;;; Deprecated.
(define-syntax struct-cloneable
  (syntax-rules ()
    [(struct-cloneable name rest ...)
     (struct name rest ...
       #:methods gen:cloneable
       [(define clone
          (lambda (val)
            (struct-copy name val)))])]))

; +---------------------+--------------------------------------------
; | The #:cloneable tag |
; +---------------------+

;;; (permit-cloneable) -> (void)
;;; Permit the "cloneable" tag.
(define-syntax-rule (permit-cloneable)
  (set-sstruct-tag! #:cloneable 0
                    (lambda (name)
                      `(#:methods gen:cloneable
                        [(define clone
                           (lambda (val) (struct-copy ,name val)))]))))
