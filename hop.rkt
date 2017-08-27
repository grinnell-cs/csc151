#lang racket

;;; File:
;;;   hop.rkt
;;; Summary:
;;;   A variety of higher-order procedures
;;; Author:
;;;   Samuel A. Rebelsky

(provide
  (contract-out
    [all (-> procedure? list? boolean?)]
    [any (-> procedure? list? boolean?)]
    [left-section (-> procedure? any/c procedure?)]
    [l-s (-> procedure? any/c procedure?)]
    [right-section (-> procedure? any/c procedure?)]
    [r-s (-> procedure? any/c procedure?)]
    ))
(provide o)
(provide section)

; +---------------------+--------------------------------------------
; | Exported procedures |
; +---------------------+

;;; Procedure:
;;;   all
;;; Parameters:
;;;   pred?, a unary predicate
;;;   lst, a list
;;; Purpose:
;;;   Determine if pred? holds for all the values in lst.
;;; Produces:
;;;   ok?, a Boolean
;;; Preconditions:
;;;   [Standard]
;;; Postconditions:
;;;   If there is an i such that (pred? (list-ref lst i))
;;;     fails to hold, then ok? is false.
;;;   Otherwise, ok? is true.
(define all
  (lambda (pred? lst)
    (or (null? lst)
        (and (pred? (car lst))
             (all pred? (cdr lst))))))

;;; Package:
;;;   csc151/hop
;;; Procedure:
;;;   any
;;; Parameters:
;;;   pred?, a unary predicate
;;;   lst, a list
;;; Purpose:
;;;   Determines if pred? holds for any of the values in lst
;;; Produces:
;;;   ok?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If there is an i s.t. (pred? (list-ref lst i)) holds, then
;;;     ok? is true.
;;;   If for all i, (pred? (list-ref list i)) does not hold, then
;;;     ok? is false.
(define any
  (lambda (pred? lst)
    (and (not (null? lst))
         (or (pred? (car lst))
             (any pred? (cdr lst))))))

;;; Package:
;;;   csc151/hop
;;; Procedure:
;;;   left-section
;;; Parameters:
;;;   proc2, a two-parameter procedure
;;;   left, a value
;;; Purpose:
;;;   Creates a one-parameter procedure by filling in the first parameter
;;;    of proc2.
;;; Produces:
;;;   proc1, a one-parameter procedure
;;; Preconditions:
;;;   left is a valid first parameter for proc2.
;;; Postconditions:
;;;   (proc1 right) = (proc2 left right)
(define left-section
  (lambda (proc2 left)
    (lambda (right) (proc2 left right))))

;;; Package:
;;;   csc151/hop
;;; Procedure:
;;;   l-s
;;; Parameters:
;;;   proc2, a two-parameter procedure
;;;   left, a value
;;; Purpose:
;;;   Creates a one-parameter procedure by filling in the first parameter
;;;    of proc2.
;;; Produces:
;;;   proc1, a one-parameter procedure
;;; Preconditions:
;;;   left is a valid first parameter for proc2.
;;; Postconditions:
;;;   (proc1 right) = (proc2 left right)
(define l-s left-section)

;;; Package:
;;;   csc151/hop
;;; Procedure:
;;;   o
;;; Parameters:
;;;   fun1, a unary function
;;;   fun2, a unary function
;;;   ...
;;;   funn, a unary function
;;; Purpose:
;;;   Compose fun1 ... funn
;;; Produces:
;;;   fun, a function
;;; Preconditions:
;;;   Each function can be applied to the results of the subsequent
;;;   function.
;;; Postconditions:
;;;   (fun x) = (fun1 (fun2 (.... (funn x)...)))
(define o
  (lambda funs
    (lambda (x)
      (let kernel ((remaining (reverse funs))
                   (val x))
        (if (null? remaining)
            val
            (kernel (cdr remaining) ((car remaining) val)))))))

;;; Package:
;;;   csc151/hop
;;; Procedure:
;;;   right-section
;;; Parameters:
;;;   proc2, a two-parameter procedure
;;;   right, a value
;;; Purpose:
;;;   Creates a one-parameter procedure by filling in the second parameter
;;;    of proc2.
;;; Produces:
;;;   proc1, a one-parameter procedure
;;; Preconditions:
;;;   left is a valid first parameter for proc2.
;;; Postconditions:
;;;   (proc1 left) = (proc2 left right)
(define right-section
  (lambda (proc2 right)
    (lambda (left) (proc2 left right))))

;;; Package:
;;;   csc151/hop
;;; Procedure:
;;;   r-s
;;; Parameters:
;;;   proc2, a two-parameter procedure
;;;   right, a value
;;; Purpose:
;;;   Creates a one-parameter procedure by filling in the second parameter
;;;    of proc2.
;;; Produces:
;;;   proc1, a one-parameter procedure
;;; Preconditions:
;;;   left is a valid first parameter for proc2.
;;; Postconditions:
;;;   (proc1 left) = (proc2 left right)
(define r-s right-section)


; +-----------------+------------------------------------------------
; | Exported macros |
; +-----------------+

;;; Package:
;;;   csc151/hop
;;; Procedure:
;;;   section
;;; Parameters:
;;;   proc, a procedure
;;;   param-1, a value or the parameter symbol (<>)
;;;   param-2, a value or the parameter symbol (<>)
;;;   ...
;;;   param-n, a value or the parameter symbol (<>)
;;; Purpose:
;;;   Create a new procedure
;;; Produces:
;;;   newproc, a procedure
;;; Preconditions:
;;;   proc accepts n parameters
;;; Postconditions:
;;;   newproc accepts one parameter for each param that is the
;;;     parameter symbol.
;;;   (newproc val1 ... valk) = (proc ...)
;;;     where the ith parameter to proc is either param-i, if param-i
;;;       is not <>, and the next element of val1...valk otherwise.
(define-syntax section
  (lambda (stx)
    (let ([info (syntax->datum stx)])
      (cond
        [(symbol? info)
         (datum->syntax stx '(quote <macro:section>))]
        [(null? (cdr info))
         (error "section: Requires a procedure")]
        [else
         (let ([sec (car info)]
               [proc (cadr info)]
               [params (cddr info)])
           (let kernel ([params params]
                        [formals null]
                        [actuals null])
             (cond
               [(null? params)
                (let ([code (list 'lambda (reverse formals)
                                  (cons proc (reverse actuals)))])
                  ; (write code) (newline) ; CHECKING
                  (datum->syntax stx code))]
               [(eq? (car params) '<>)
                (let ([formal (gensym)])
                  (kernel (cdr params)
                          (cons formal formals)
                          (cons formal actuals)))]
               [else 
                (kernel (cdr params)
                        formals
                        (cons (car params) actuals))])))]))))

