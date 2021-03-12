#lang racket

;;; File:
;;;   binary-trees-list.rkt
;;; Summary:
;;;   Binary trees implemented as lists.
;;; Author:
;;;   Samuel A. Rebelsky

(require csc151/hop)
(require racket/match)

(provide (all-defined-out))

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

;;; (leaf? t) -> boolean?
;;;   t : any?
;;; Returns true iff t is the leaf of a binary search tree.
(define leaf?
  (lambda (t)
    (and (binary-tree-node? t)
         (empty-tree? (btl t))
         (empty-tree? (btr t)))))

;;; empty-tree : tree?
;;; An alias for the empty binary tree.
(define empty-tree (string->symbol "\u25EC")) ; empty triangle with dot
; (define empty-tree (string->symbol "\u2022")) ; bullet
; (define empty-tree (string->symbol "\u25B5")) ; empty triangle

;;; (empty-tree? t) -> boolean?
;;;   t : value
;;; Returns true iff is t is an empty binary tree.
(define empty-tree? 
  (lambda (t)
    (eq? t empty-tree)))

;;; (binary-tree value left right) -> tree?
;;;   value : any
;;;   left : tree?
;;;   right : tree?
;;; Returns a non-empty tree with value at the root
;;; and the given left and right subtrees.
(define binary-tree list)

;;; (leaf value) -> tree?
;;;   value : any
;;; Returns a non-empty tree with no children (i.e., a leaf)
;;; with value at the root.
(define leaf
  (section binary-tree <> empty-tree empty-tree))

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

;;; Shorthands
(define btn binary-tree) ; n for "new" or "node"
(define btt binary-tree-top)
(define btl binary-tree-left)
(define btr binary-tree-right)

;;; (display-binary-tree t) -> void?
;;;   t : tree?
;;; Prints tree t to the console in a well-formatted manner.
(define display-binary-tree
  (lambda (t)
    (let* (; The different levels of bullets
           [bullets (vector "\u29BF " ; circled bullet
                            "\u25cf " ; black circle
                            "\u25b6 " ; black triangle
                            "\u25aa " ; black square
                            "\u25c6 " ; Replacements cover of Kiss song
                            "\u25cb " ; white circle
                            "\u25b7 " ; white triangle
                            "\u25ab " ; white square
                            "\u25c7 " ; white diamond
                            "\u2043 " ; dash
                            "\u2219 ")]
           ; The index of the last bullet
           [last-bullet (- (vector-length bullets) 1)]
           ; Choose the appropriate bullet for a level.
           [bullet
            (lambda (level)
              (vector-ref bullets (min level last-bullet)))])
      (letrec (; Display a binary tree at the appropriate indentation
               [helper
                (lambda (t indent)
                  (when (not (empty-tree? t))
                    (display (make-string (* indent 2) #\space))
                    (display (bullet indent))
                    (display (btt t))
                    (newline)
                    (helper (btl t) (+ indent 1))
                    (helper (btr t) (+ indent 1))))])
        (helper t 0)))))

;;; (bt-traverse path tree) -> binary-tree?
;;;   path : string? (contains only r's and l's)
;;;   bree : binary-tree?
;;; Follow the path given by the path and return the described
;;; value.
(define bt-traverse
  (lambda (path tree)
    (cond
      [(equal? path "")
       tree]
      [(empty-tree? tree)
       (error "You've run off the end of the tree")]
      [(char=? #\l (string-ref path 0))
       (bt-traverse (substring path 1)
                    (binary-tree-left tree))]
      [(char=? #\r (string-ref path 0))
       (bt-traverse (substring path 1)
                    (binary-tree-right tree))]
      [else
       (error "Invalid subpath" path)])))

;;; (vector->tree vec) -> binary-tree?
;;;    vec : vector
;;; Converts a vector into a relatively balanced binary tree.
(define vector->tree
  ; The kernel builds a tree from the elements at positions lb (inclusive)
  ; to ub (exclusive)
  (letrec ([helper (lambda (vec lb ub)
                     (if (<= ub lb)
                         empty-tree
                         (let [(mid (quotient (+ lb ub) 2))]
                           (btn (vector-ref vec mid)
                                (helper vec lb mid)
                                (helper vec (+ 1 mid) ub)))))])
    (lambda (vec)
      (helper vec 0 (vector-length vec)))))

;;; (random-tree n rproc) -> binary-tree?
;;;   n : non-negative integer
;;;   rproc : procedure? (0 parameters)
;;; Create a random tree of size n.
(define random-tree
  (lambda (n rproc)
    (if (zero? n)
        empty-tree
        (let ([left-size (random n)])
          (binary-tree (rproc)
                       (random-tree left-size rproc)
                       (random-tree (- n left-size 1) rproc))))))

;;; (binary-tree-contains? tree val) -> boolean?
;;;   tree : binary-tree?
;;;   val : any?
;;; Determine if val appears somewhere in tree.
(define binary-tree-contains?
  (lambda (tree val)
    (and (not (empty-tree? tree))
         (let ([root (btt tree)])
           ; (display root) (newline)
           (or (equal? root val)
               (binary-tree-contains? (btl tree) val)
               (binary-tree-contains? (btr tree) val))))))

;;; (binary-tree-depth tree) -> integer?
;;;   tree : binary-tree?
;;; Determine the number of levels in the tree
(define binary-tree-depth
  (lambda (tree)
    (if (empty-tree? tree)
        0
        (+ 1 (max (binary-tree-depth (btl tree))
                  (binary-tree-depth (btr tree)))))))

;;; (binary-tree-size tree) -> integer?
;;;   tree : binary-tree?
;;; Determine the number of elements in a tree
(define binary-tree-size
  (lambda (tree)
    (if (empty-tree? tree)
        0
        (+ 1
           (binary-tree-size (btl tree))
           (binary-tree-size (btr tree))))))

;;; (bst-find-string bst str) -> string
;;;   bst : binary-search-tree?
;;;   str : string
;;; Find str (case insensitive) in bst.  If str is not in bst, returns #f.
(define bst-find-string
  (lambda (bst str)
    (if (empty-tree? bst)
        #f
        (let ([root (btt bst)])
          ; (display root) (newline)
          (cond
            [(string-ci=? str root)
             root]
            [(string-ci<? str root)
             (bst-find-string (btl bst) str)]
            [else
             (bst-find-string (btr bst) str)])))))
