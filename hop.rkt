#lang racket

;;; File:
;;;   hop.rkt
;;; Summary:
;;;   A variety of higher-order procedures
;;; Author:
;;;   Samuel A. Rebelsky
;;;   Peter-Michael Osera

(require scribble/srcdoc
         (for-doc racket/base scribble/manual))

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
(provide
  (proc-doc/names
    all (procedure? list? . -> . boolean?) (pred lst)
    ("Determines if " (racket pred) " holds for all values in " (racket lst) ".")))
(define all
  (lambda (pred lst)
    (or (null? lst)
        (and (pred (car lst))
             (all pred (cdr lst))))))

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
(provide
  (proc-doc/names
    any (procedure? list? . -> . boolean?) (pred lst)
    ("Determines if " (racket pred) " holds for any of the values in " (racket lst) ".")))
(define any
  (lambda (pred lst)
    (and (not (null? lst))
         (or (pred (car lst))
             (any pred (cdr lst))))))

;;; Package:
;;;   csc151/hop
;;; Procedure:
;;;   comparator
;;; Parameters:
;;;   compare?, a binary comparator
;;;   extract, a unary procedure
;;; Purpose:
;;;   Creates a comparator that takes two values, applies extract
;;;   to each, and then compares the results of both.
;;; Produces:
;;;   comp?, a binary comparator
;;; Preconditions:
;;;   compare? can be applied to the results of extract.
;;; Postconditions:
;;;   (comp? v1 v2) = (compare? (extract v1) (extract v2))
(provide
  (proc-doc/names
    comparator (procedure? procedure? . -> . (any/c any/c . -> . boolean?)) (compare extract)
    ("Creates a comparator that takes two values, applies " (racket extract)
     "to each, and then compares the results of both using " (racket compare) ".")))
(define comparator
  (lambda (compare extract)
    (lambda (v1 v2)
      (compare (extract v1) (extract v2)))))

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
(provide
  (proc-doc/names
    left-section (procedure? any/c . -> . (any/c . -> . any/c)) (proc2 left)
    ("Creates a one-parameter procedure by filling in the first parameter of " (racket proc2))))
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
(provide
  (thing-doc
    l-s (procedure? any/c . -> . (any/c . -> . any/c))
    ("An alias for " (racket left-section))))
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
(provide
  (thing-doc
    o procedure?
    ("An alias for " (racket compose))))
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
(provide
  (proc-doc/names
    right-section (procedure? any/c . -> . (any/c . -> . any/c)) (proc2 right)
    ("Creates a one-parameter procedure by filling in the second parameter of " (racket proc2))))
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
(provide
  (thing-doc
    r-s (procedure? any/c . -> . (any/c . -> . any/c))
    ("An alias for " (racket right-section))))
(define r-s right-section)

;;; Package:
;;;   csc151/hop
;;; Procedure:
;;;   tally-value
;;; Parameters:
;;;   l, a list
;;;   v, a value
;;; Purpose:
;;;   Counts the number of occurrences of v in l.
;;; Produces:
;;;   result, the number of occurrences of v in l.
;;; Preconditions:
;;;   [No additional preconditions]
;;; Postconditions:
;;;   (<= 0 result (length l))
(provide
  (proc-doc/names
    tally-value (list? any/c . -> . integer?) (lst value)
    ("Returns the number of occurrences of " (racket value) " in " (racket lst) ".")))
(define tally-value
  (lambda (lst value)
    (foldl (lambda (x acc)
             (if (equal? x value)
                 (+ 1 acc)
                 acc))
           0 lst)))

;;; Package:
;;;   csc151/hop
;;; Procedure:
;;;   :>
;;; Parameters:
;;;   x, a value of type T
;;;   fs, a list of functions
;;; Purpose:
;;;   The pipeline operator.  Chains invocations of functions found
;;;   in fs in a left-to-right fashion starting with the value x.
;;; Produces:
;;;   result, a value
;;; Preconditions:
;;;   The first element of fs is a function of type T -> U1.
;;;   The ith element of fs is a function of type T(i-1) -> Ti.
;;; Postconditions:
;;;   result has type Tk where Tk is the output type of the final
;;;   function of fs.
(provide
  (proc-doc/names
    :> (-> any/c procedure? ... any/c) (x f fs)
    ("The pipeline operator.  Chains invocations of functions " (racket f)
     " and " (racket fs) " in a left-to-right fashion starting with the value " (racket x))))
(define :>
  (lambda (x . fs)
    (foldl (lambda (g acc) (g acc)) x fs)))

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
(provide section)
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
