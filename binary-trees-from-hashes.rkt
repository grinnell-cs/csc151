#lang racket

;;; File:
;;;   binary-trees-from-hashes.rkt
;;; Summary:
;;;   Binary trees implemented as Racket hashes.
;;; Author:
;;;   Samuel A. Rebelsky

(require csc151/hop)
(require racket/include)

(provide (all-defined-out))

; +-----------------------------+------------------------------------
; | Binary tree node structures |
; +-----------------------------+

;;; (binary-tree-node? t) -> boolean?
;;;   t : any?
;;; Returns true iff t is a node in a tree.  In the current implementation,
;;; that's a hash table with keys of value, left, and right.
(define binary-tree-node?
  (lambda (t)
    (and (hash? t)
         (hash-has-key? t 'value)
         (hash-has-key? t 'left)
         (hash-has-key? t 'right))))

;;; (binary-tree? t) -> boolean?
;;;   t : any?
;;; Returns true iff t is a binary tree.
(define binary-tree?
  (lambda (t)
    (or (empty-tree? t)
        (and (binary-tree-node? t)
             (binary-tree? (hash-ref t 'left))
             (binary-tree? (hash-ref t 'right))))))

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
(define binary-tree 
  (lambda (value left right)
    (let ([t (make-hash)])
      (hash-set! t 'value value)
      (hash-set! t 'left left)
      (hash-set! t 'right right)
      t)))

;;; (binary-tree-top t) -> any
;;;   t : tree?, (not (empty-tree? t))
;;; Returns the value at the root of this tree.
(define binary-tree-top
  (lambda (t)
    (hash-ref t 'value)))

;;; (binary-tree-left t) -> any
;;;   t : tree?, (not (empty-tree? t))
;;; Returns the left child of this tree.
(define binary-tree-left
  (lambda (t)
    (hash-ref t 'left)))

;;; (binary-tree-right t) -> any
;;;   t : tree?, (not (empty-tree? t))
;;; Returns the right child of this tree.
(define binary-tree-right
  (lambda (t)
    (hash-ref t 'right)))

; +-------------+----------------------------------------------------
; | Common code |
; +-------------+

(include "includes/binary-trees-common.inc")

