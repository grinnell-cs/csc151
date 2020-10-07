#lang racket

;;; File:
;;;   trees.rkt
;;; Summary:
;;;   Tree functions for CSC 151.
;;; Author:
;;;   Samuel A. Rebelsky
;;;   Peter-Michael Osera

(require scribble/srcdoc
         (for-doc racket/base scribble/manual))

(provide
  (proc-doc/names
    tree? (-> any/c boolean?) (t)
    ("Returns true iff " (racket t) " is a binary tree.")))
(define tree?
  (lambda (t)
    (or (null? t)
        (and (pair? t)
             (pair? (cdr t))
             (tree? (cadr t))
             (tree? (cddr t))))))

(provide
  (thing-doc
    empty tree?
    ("The empty tree")))
(define empty-tree null)

(provide
  (proc-doc/names
    empty-tree? (-> tree? boolean?) (t)
    ("Returns true iff tree " (racket t) " is an empty tree.")))
(define empty-tree? null?)

(provide
  (proc-doc/names
    tree (-> any/c tree? tree? tree?) (value left right)
    ("Returns a non-empty tree with " (racket value) " at the root and the given " (racket left) " and " (racket right) " subtrees.")))
(define tree
  (lambda (value left right)
    (cons value (cons left right))))

(provide
  (proc-doc/names
    leaf (-> any/c tree?) (value)
    ("Returns a non-empty tree with no children (i.e., a leaf) with " (racket value) " at the root.")))
(define leaf
  (lambda (value)
    (tree value empty-tree empty-tree)))

(provide
  (proc-doc/names
    value-of (-> tree? any) (t)
    ("Returns the value at the root of tree " (racket t) ". Assumes " (racket t) " is non-empty.")))
(define value-of car)

(provide
  (proc-doc/names
    left-child-of (-> tree? any) (t)
    ("Returns the left child tree " (racket t) ". Assumes " (racket t) " is non-empty.")))
(define left-child-of cadr)

(provide
  (proc-doc/names
    right-child-of (-> tree? any) (t)
    ("Returns the right child tree " (racket t) ". Assumes " (racket t) " is non-empty.")))
(define right-child-of cddr)

;;; (display-tree t) -> void?
;;; t : tree?
;;;
;;; Prints tree t to the console in a well-formatted manner.

(provide
  (proc-doc/names
    display-tree (-> tree? void?) (t)
    ("Prints tree " (racket t) " to the console in a well-formatted manner.")))
(define display-tree
  (lambda (t)
    (letrec ([bullet
              (lambda (level)
                (cond [(= level 0) "* "]
                      [(= level 1) "+ "]
                      [else        "- "]))]
             [go
              (lambda (t indent)
                (if (empty-tree? t)
                    (void)
                    (begin
                      (display (make-string (* indent 2) #\space))
                      (display (bullet indent))
                      (display (value-of t))
                      (newline)
                      (go (left-child-of t) (+ indent 1))
                      (go (right-child-of t) (+ indent 1)))))])
      (go t 0))))
