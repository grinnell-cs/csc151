#lang racket

;;; File:
;;;   type-predicates.rkt
;;; Author:
;;;   Samuel A. Rebelsky
;;; Summary:
;;;   Some simple utilities for dealing with types.

(provide (all-defined-out))

;;; (any? val) -> true?
;;;   val : any?
;;; Returns true for every parameter.
(define any?
  (lambda (val)
    #t))

;;; (any-of pred_1? ... pred_n?) -> unary-predicate?
;;;   pred_1? : unary-predicate?
;;;   ...
;;;   pred_n? : unary-predicate?
;;; Returns a predicate of one parameter, `val`, that checks whether any
;;; of `pred_1?` ... `pred_n?` hold on `val`.
(define any-of
  (letrec ([kernel (lambda (preds val)
                     (and (not (null? preds))
                          (or ((car preds) val)
                              (kernel (cdr preds) val))))])
    (lambda preds
      (lambda (val)
        (kernel preds val)))))

;;; (all-of pred_1? ... pred_n?) -> unary-predicate?
;;;   pred_1? : unary_predicate?
;;;   ...
;;;   pred_n? : unary-predicate
;;; Returns a predicate of one parameter, `val`, that checks whether all
;;; of `pred_1?` ... `pred_n?` hold on `val`.
(define all-of
  (letrec ([kernel (lambda (preds val)
                     (or (null? preds)
                         (and ((car preds) val)
                              (kernel (cdr preds) val))))])
    (lambda preds
      (lambda (val)
        (kernel preds val)))))

;;; (list-of pred?) -> unary-predicate?
;;;   pred? : unary-predicate?
;;; Builds a unary predicate that verifies that its parameter is a list,
;;; all of whose elements match `pred?`.
(define list-of listof)

;;; (vector-of pred?) -> unary-predicate?
;;;   pred? : unary-predicate?
;;; Builds a unary predicate that verifies that its parameter is a vector
;;; all of whose elements match `pred?`.
(define vector-of vectorof)

;;; (nonnegative? val) -> boolean?
;;;   val : real?
;;; Detrmines if `val` is non-negative (greater than or equal to 0).
(define nonnegative?
  (lambda (val)
    (>= val 0)))

;;; (nonnegative-real? val) -> boolean?
;;;   val : any?
;;; Determines if val is a nonnegative real number.
(define nonnegative-real?
  (all-of real? nonnegative?))

;;; (one-of v1 ... vn) -> unary-predicate?
;;;   v1 : any?
;;;   ...
;;;   vn : any?
;;; Returns a predicate that returns true if its parameter is equal
;;; to any of v1 through vn.
(define one-of
  (lambda vals
    (lambda (val)
      (and (member val vals) #t))))

