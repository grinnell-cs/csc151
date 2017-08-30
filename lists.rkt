#lang racket

;;; File:
;;;   lists.rkt
;;; Summary:
;;;   A variety of procedure associated with lists.
;;; Author:
;;;   Samuel A. Rebelsky

(provide
  (contract-out
    [index-of (-> any/c list? integer?)]
    [iota (-> natural-number/c (listof integer?))]
    [map1 (-> (-> any/c any) list? list?)]
    [reduce (-> (-> any/c any/c any) list? any/c)]
    [reduce-left (-> (-> any/c any/c any) list? any/c)]
    [reduce-right (-> (-> any/c any/c any) list? any/c)]
    ))

; +-------------------------------+----------------------------------
; | Private procedures and values |
; +-------------------------------+

;;; Constant:
;;;   FIRST-FIRST-ODDS
;;; Type:
;;;   real
;;; Summary:
;;;   The odds that we process the first element first when mapping
;;;   through a list.
(define FIRST-FIRST-ODDS 0.5)

; +---------------------+--------------------------------------------
; | Exported procedures |
; +---------------------+

;;; Package:
;;;   csc151/lists
;;; Procedure:
;;;   index-of
;;; Parameters:
;;;   val, a Scheme val
;;;   lst, a list
;;; Purpose:
;;;   Find the index of val in lst
;;; Produces:
;;;   index, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   if index is -1, val does not appear in lst.
;;;   Otherwise,
;;;     (list-ref lst index) is val
;;;     For no i smaller than index is (list-ref lst i) val.
(define index-of
  (lambda (val lst)
    (let kernel ([pos 0]
                 [remaining lst])
      (cond
        [(null? remaining)
         -1]
        [(equal? val (car remaining))
         pos]
        [else
         (kernel (+ pos 1) (cdr remaining))]))))


;;; Package:
;;;   csc151/lists
;;; Procedure:
;;;   iota
;;; Parameters:
;;;   n, a positive integer
;;; Purpose:
;;;   Create a list of all non-negative integers less than n.
;;; Produces:
;;;   ints, a list of integers
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (length ints) = n
;;;   ints has the form (0 1 2 3 ...)
(define iota
  (lambda (n)
    (let kernel ([i 0])
      (if (= i n)
          null
          (cons i (kernel (+ i 1)))))))

;;; Package:
;;;   csc151/lists
;;; Procedure:
;;;   map1
;;; Parameters:
;;;   fun, a procedure
;;;   lst, a list of values
;;; Purpose:
;;;   Creates a new list by applying fun to each element of lst.
;;; Produces:
;;;   newlst, a list
;;; Preconditions:
;;;   fun must be applicable to each element of lst.
;;; Postconditions:
(define map1
  (lambda (fun lst)
    (cond
      [(null? lst)
       null]
      [(null? (cdr lst))
       (cons (fun (car lst)) null)]
      [(< (random) FIRST-FIRST-ODDS)
       (let* ([first (fun (car lst))]
              [rest (map1 fun (cdr lst))])
         (cons first rest))]
      [else
       (let* ([rest (map1 fun (cdr lst))]
              [first (fun (car lst))])
         (cons first rest))])))

;;; Package:
;;;   csc151/lists
;;; Procedure:
;;;   reduce
;;; Parameters:
;;;   op, a binary procedure
;;;   lst, a list of values of the form (val1 val2 ... valn)
;;; Purpose:
;;;   Creates a new value by repeatedly applying op to the values
;;;   in lst.
;;; Produces:
;;;   result, a value
;;; Preconditions:
;;;   op must be applicable to pairs of elements of lst.
;;;   op must return the same types that it takes as input
;;; Postconditions:
;;;   In infix notation, result is val1 op val2 op val3 ... op valn
;;;   the order of the evalution of the operations is undefined
(define reduce
  (lambda (fun lst)
    (cond
      [(null? (cdr lst))
       (car lst)]
      [(< (random) FIRST-FIRST-ODDS)
       (reduce fun (cons (fun (car lst) (cadr lst))
                         (cddr lst)))]
      [else
       (fun (car lst) (reduce fun (cdr lst)))])))

;;; Package:
;;;   csc151/lists
;;; Procedure:
;;;   reduce-left
;;; Parameters:
;;;   op, a binary procedure
;;;   lst, a list of values of the form (val1 val2 ... valn)
;;; Purpose:
;;;   Creates a new value by repeatedly applying op to the values
;;;   in lst.
;;; Produces:
;;;   result, a value
;;; Preconditions:
;;;   op must be applicable to pairs of elements of lst.
;;;   op must return the same types that it takes as input
;;; Postconditions:
;;;   result is (op (op (op val1 val2) val3) ... valn)
(define reduce-left
  (lambda (fun lst)
    (cond
      [(null? (cdr lst))
       (car lst)]
      [else
       (reduce-left fun (cons (fun (car lst) (cadr lst))
                              (cddr lst)))])))

;;; Package:
;;;   csc151/lists
;;; Procedure:
;;;   reduce-right
;;; Parameters:
;;;   op, a binary procedure
;;;   lst, a list of values of the form (val1 val2 ... valn)
;;; Purpose:
;;;   Creates a new value by repeatedly applying op to the values
;;;   in lst.
;;; Produces:
;;;   result, a value
;;; Preconditions:
;;;   op must be applicable to pairs of elements of lst.
;;;   op must return the same types that it takes as input
;;; Postconditions:
;;;   result is (op val1 (op val2 (op val3 ... (op valn-1 valn))))
(define reduce-right
  (lambda (fun lst)
    (cond
      [(null? (cdr lst))
       (car lst)]
      [else
       (fun (car lst) (reduce-right fun (cdr lst)))])))

