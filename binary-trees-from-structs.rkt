#lang racket

;;; File:
;;;   binary-trees-from-structs.rkt
;;; Summary:
;;;   Binary trees implemented as Racket structs.
;;; Author:
;;;   Samuel A. Rebelsky

(require csc151/hop)
(require racket/match)
(require racket/include)

(provide (all-defined-out))

; +-----------------------------+------------------------------------
; | Binary tree node structures |
; +-----------------------------+

(struct binary-tree-node (top left right)
  #:transparent
  #:reflection-name 'binary-tree
  #:guard 
  (lambda (top left right name)
    (cond
      [(not (binary-tree? left))
       (error "binary-tree: Expects a binary tree as second parameter, received"
              left)]
      [(not (binary-tree? right))
       (error "binary-tree: Expects a binary tree as third parameter, received"
              right)]
      [else
       (values top left right)])))

; +-----------------------+------------------------------------------
; | Empty tree structures |
; +-----------------------+

(struct empty-tree ()
  #:transparent)

; +--------------------+---------------------------------------------
; | Primary Procedures |
; +--------------------+

;;; (binary-tree value left right) -> binary-tree?
;;;    value : any?
;;;    left: binary-tree?
;;;    right: binary-tree?
;;; Build a new binary tree.
(define binary-tree binary-tree-node)

;;; (binary-tree val) -> boolean?
;;;   val : any?
;;; Returns true iff val is a binary tree.
(define binary-tree?
  (lambda (val)
    (or (empty-tree? val)
        (binary-tree-node? val))))

;;; (binary-tree-top t) -> any
;;;   t : tree?, (not (empty-tree? t))
;;; Returns the value at the root of this tree.
(define binary-tree-top binary-tree-node-top)

;;; (binary-tree-left t) -> any
;;;   t : tree?, (not (empty-tree? t))
;;; Returns the left child of this tree.
(define binary-tree-left binary-tree-node-left)

;;; (binary-tree-right t) -> any
;;;   t : tree?, (not (empty-tree? t))
;;; Returns the right child of this tree.
(define binary-tree-right binary-tree-node-right)

; +-------------+----------------------------------------------------
; | Common code |
; +-------------+

(include "includes/binary-trees-common.inc")
