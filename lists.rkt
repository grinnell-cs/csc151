#lang racket

;;; File:
;;;   lists.rkt
;;; Summary:
;;;   A variety of procedure associated with lists.
;;; Author:
;;;   Samuel A. Rebelsky

(provide
  (contract-out
    [iota (-> natural-number/c (listof integer?))]
    [map1 (-> (-> any/c any) list? list?)]
    [reduce (-> (-> any/c any/c any) list? any/c)]
    [reduce-left (-> (-> any/c any/c any) list? any/c)]
    [reduce-right (-> (-> any/c any/c any) list? any/c)]
    [tally-all (-> list? list?)]
    [take-random (-> list? integer? list?)]
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

;;; Procedure:
;;;   tally-all
;;; Parameters:
;;;   lst, a list of values
;;; Purpose:
;;;   Tallies all of the values in lst
;;; Produces:
;;;   tallies, a list of (key count) lists.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   * If val appears k times in lst, then (val k) appears in tallies
;;;   * If (val k) appears in tallies, then val appears k times in lst
;;;   * Each value in lst is the car of exactly one list in tallies.
(define tally-all
  (let ([update-tallies
         (lambda (val tallies)
           (let kernel [(remaining tallies)]
             (if (null? remaining)
                 (cons (vector val 1) tallies)
                 (let ([current-tally (car remaining)])
                   (cond
                     [(equal? val (vector-ref current-tally 0))
                      (vector-set! current-tally 1 
                                   (+ (vector-ref current-tally 1) 1))
                      tallies]
                     [else
                       (kernel (cdr remaining))])))))])
    (lambda (lst)
      (let kernel ([rest lst]
                   [tallies null])
        (if (null? rest)
            (map vector->list (reverse tallies))
            (kernel (cdr rest)
                    (update-tallies (car rest) tallies)))))))

;;; Procedure:
;;;   take-random
;;; Parameters:
;;;   lst, a list
;;;   n, a non-negative integer
;;; Purpose:
;;;   Grab n "randomly selected elements" from lst
;;; Produces:
;;;   elements, a list
;;; Preconditions:
;;;   n <= (length lst)
;;; Postconditions:
;;;   (length elements) = n
;;;   Every element in elements appears in lst.
;;;   Every element in elements represents a separate element of lst.
(define take-random
  (lambda (lst n)
    (let kernel ([remaining lst]
                 [len (length lst)]
                 [n n])
      (cond
        [(or (= n 0) (= len 0))
         null]
        [(<= (random) (/ n len))
         (cons (car remaining)
               (kernel (cdr remaining)
                       (- len 1)
                       (- n 1)))]
        [else        
         (kernel (cdr remaining)
                 (- len 1)         
                 n)]))))
