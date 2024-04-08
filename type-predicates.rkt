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
(define vector-of 
  (lambda (pred?)
    (lambda (val)
      (and (vector? val)
           (let kernel ([pos (- (vector-length val) 1)])
             (or (< pos 0)
                 (and (pred? (vector-ref val pos))
                      (kernel (- pos 1)))))))))

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

;;; (pair-of pred1? pred2?) -> unary-predicate?
;;;   pred1? : unary-predicate?
;;;   pred2? : unary-predicate?
;;; Returns a new predicate that checks if its parameter is a pair
;;; whose car satisfies `pred1?` and whose cdr satisfies `pred2?`.
(define pair-of
  (lambda (pred1? pred2?)
    (lambda (val)
      (and (pair? val)
           (pred1? (car val))
           (pred2? (cdr val))))))

;;; (positive-real? val) -> boolean?
;;;   val : any?
;;; Determines if `val` is a positive real number.
(define positive-real?
  (all-of real? positive?))

;;; (param-error proc-name param-type param-num param-name value) -> void?
;;;   proc-name : symbol?
;;;   param-type : symbol?
;;;   param-num : positive-integer?
;;;   param-name : symbol?
;;;   value : any
;;; Issue an error.
(define param-error
  (lambda (proc-name param-type param-num param-name value)
    (error proc-name
           "expects ~a for parameter ~a (~a), received ~a"
           param-type
           param-num
           param-name
           value)))

;;; (param-check! proc num type? param) -> void?
;;;   proc-name : identifier?
;;;   param-num : positive-integer?
;;;   type? : predicate?
;;;   param : identifier?
;;; Checks a parameter (I think).
(define-syntax-rule (param-check! proc num type? param)
  (when (not (type? param))
    (error (quote proc)
           "expects ~a for parameter ~a (~a), received ~a"
           (quote type?)
           num
           (quote param)
           param)))

;;; (greater-than num) -> procedure?
;;;   num : real?
;;; Returns a procedure that hold only when its parameter is greater
;;; than `num`.
(define greater-than
  (lambda (num)
    (lambda (val)
      (> val num))))

;;; (greater-equal num) -> procedure?
;;;   num : real?
;;; Returns a procedure that hold only when its parameter is greater
;;; than or equal to `num`.
(define greater-equal
  (lambda (num)
    (lambda (val)
      (>= val num))))

;;; (at-least num) -> procedure?
;;;   num : real?
;;; Returns a procedure that holds only when its parameter is at
;;; least `num` (i.e., greater than or equal to `num`).
(define at-least greater-equal)

;;; (less-than num) -> procedure?
;;;   num : real?
;;; Returns a procedure that hold only when its parameter is less
;;; than `num`.
(define less-than
  (lambda (num)
    (lambda (val)
      (< val num))))

;;; (less-equal num) -> procedure?
;;;   num : real?
;;; Returns a procedure that holds only when its parameter is less
;;; than or equal to `num`.
(define less-equal
  (lambda (num)
    (lambda (val)
      (<= val num))))

;;; (at-most num) -> procedure?
;;;   num : real?
;;; Returns a procedure that holds only when its parameter is at
;;; most `num` (i.e., less than or equal to `num`).
(define at-most less-equal)

;;; (has-length len) -> predicate?
;;;   len : non-negative-integer?
;;; Returns a predicate that holds only when its parameter is a list
;;; of the specified length or a vector of the specified length.
(define has-length
  (lambda (len)
    (lambda (val)
      (or (and (list? val) (= len (length val)))
          (and (vector? val) (= len (vector-length val)))))))

;;; (nonempty? lst) -> boolean?
;;;   lst : list?
;;; Determine if lst is nonempty.
(define nonempty?
  (lambda (lst)
    (not (null? lst))))

;;; (nonempty-vector? vec) -> boolean?
;;;   vec : vector?
;;; Determine if a vector is nonempty.
(define nonempty-vector?
  (lambda (vec)
    (> (vector-length vec) 0)))
