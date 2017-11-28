#lang racket

;;; File:
;;;   trees.rkt
;;; Summary:
;;;   Tree functions for CSC 151.
;;; Author:
;;;   Samuel A. Rebelsky

(require csc151/hop)

(provide (all-defined-out))

; +-------------------+----------------------------------------------
; | Tree Constructors |
; +-------------------+

;;; Name:
;;;   empty
;;; Type:
;;;   tree
;;; Value:
;;;   The empty tree
(define empty 'empty)

;;; Procedure:
;;;   leaf
;;; Parameters:
;;;   val, a value
;;; Purpose:
;;;   Make a leaf node.
;;; Produces:
;;;   tree, a tree
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (contents tree) = val
;;;   (left tree) = empty
;;;   (right tree) = empty
(define leaf
  (lambda (val)
    (node val empty empty)))

;;; Procedure:
;;;   node
;;; Parameters:
;;;   val, a value
;;;   left-subtree, a tree
;;;   right-subtree, a tree
;;; Purpose:
;;;   Create a node in a binary tree.
;;; Produces:
;;;   tree, a tree
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (node? tree) holds.
;;;   (left tree) = left-subtree.
;;;   (right tree) = right-subtree.
;;;   (contents tree) = val.
(define node
  (lambda (val left right)
    (vector 'node val left right)))

; +----------------+-------------------------------------------------
; | Tree Observers |
; +----------------+

;;; Procedure:
;;;   contents
;;; Parameters:
;;;   nod, a binary tree node
;;; Purpose:
;;;   Extract the contents of node.
;;; Produces:
;;;   val, a value
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (contents (node val l r)) = val
(define contents
  (lambda (nod)
    (cond
      [(not (node? nod))
       (error "contents requires a node, received" nod)]
      [else
       (vector-ref nod 1)])))

;;; Procedure:
;;;   left
;;; Parameters:
;;;   nod, a binary tree node
;;; Purpose:
;;;   Extract the left subtree of nod.
;;; Produces:
;;;   l, a tree
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (left (node val l r)) = l
(define left
  (lambda (nod)
    (cond
      [(not (node? nod))
       (error "left requires a node, received" nod)]
      [else
       (vector-ref nod 2)])))

;;; Procedure:
;;;   right
;;; Parameters:
;;;   nod, a binary tree node
;;; Purpose:
;;;   Extract the right subtree of nod.
;;; Produces:
;;;   r, a tree
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (right (node val l r)) = r
(define right
  (lambda (nod)
    (cond
      [(not (node? nod))
       (error "right requires a node, received" nod)]
      [else
       (vector-ref nod 3)])))

; +-----------------+------------------------------------------------
; | Tree Predicates |
; +-----------------+

;;; Procedure:
;;;   empty?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val represents an empty tree.
;;; Produces:
;;;   is-empty?, a Boolean 
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   is-empty? is true (#t) if and only if val can be interpreted as
;;;   the empty tree.
(define empty? 
  (section eq? <> empty))

;;; Procedure:
;;;   leaf?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val is a tree leaf
;;; Produces:
;;;   is-leaf?, a Boolean
(define leaf?
  (lambda (val)
    (and (node? val)
         (empty? (left val))
         (empty? (right val)))))

;;; Procedure:
;;;   node?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val can be used as a tree node.
;;; Produces:
;;;   is-node?, a Boolean
(define node?
  (lambda (val)
    (and (vector? val)
         (= (vector-length val) 4)
         (eq? (vector-ref val 0) 'node))))

