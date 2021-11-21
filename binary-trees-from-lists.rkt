#lang racket

;;; File:
;;;   binary-trees-from-lists.rkt
;;; Summary:
;;;   Binary trees implemented as lists.
;;; Author:
;;;   Samuel A. Rebelsky

(require csc151/hop)
(require racket/match)
(require racket/include)

(provide (all-defined-out))

; +--------------------+---------------------------------------------
; | Primary procedures |
; +--------------------+

;;; (binary-tree-node? t) -> boolean?
;;;   t : any?
;;; Returns true iff t is a node in a tree.  In the current implementation,
;;; that's a list of length three.
(define binary-tree-node?
  (lambda (t)
    (and (pair? t)
         (pair? (cdr t))
         (pair? (cddr t))
         (null? (cdddr t)))))
         
;;; (binary-tree? t) -> boolean?
;;;   t : any?
;;; Returns true iff t is a binary tree.
(define binary-tree?
  (lambda (t)
    (or (empty-tree? t)                    ; the empty tree is a tree
        (and (binary-tree-node? t)         ; A node in the tree
             (binary-tree? (cadr t))       ; Where element 1 is a tree
             (binary-tree? (caddr t))))))  ; And element 2 is a tree

;;; empty-tree : tree?
;;; An alias for the empty binary tree.
(define empty-tree 
  (lambda ()
    (string->symbol "\u25EC"))) ; empty triangle with dot

;;; (empty-tree? t) -> boolean?
;;;   t : value
;;; Returns true iff is t is an empty binary tree.
(define empty-tree? 
  (lambda (t)
    (eq? t (empty-tree))))

;;; (binary-tree value left right) -> tree?
;;;   value : any
;;;   left : tree?
;;;   right : tree?
;;; Returns a non-empty tree with value at the root
;;; and the given left and right subt/rees.
(define binary-tree list)

;;; (binary-tree-top t) -> any
;;;   t : tree?, (not (empty-tree? t))
;;; Returns the value at the root of this tree.
(define binary-tree-top car)

;;; (binary-tree-left t) -> any
;;;   t : tree?, (not (empty-tree? t))
;;; Returns the left child of this tree.
(define binary-tree-left cadr)

;;; (binary-tree-right t) -> any
;;;   t : tree?, (not (empty-tree? t))
;;; Returns the right child of this tree.
(define binary-tree-right caddr)

; +-------------+----------------------------------------------------
; | Common code |
; +-------------+

(include "includes/binary-trees-common.inc")
