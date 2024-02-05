#lang racket

;;; File:
;;;   point.rkt
;;; Summary:
;;;   A variety of procedures for working with points.
;;; Author:
;;;   Samuel A. Rebelsky
;;;
;;; IMPORTANT: If you are going to edit this file (or any of the
;;; imported files), make sure that you've updated the DrRacket editor
;;; to treat `sstruct` like `struct`.
;;;
;;; IMPORTANT: In order to import this file as a file (rather than through
;;; the csc151 library, you will likely need to require ssstruct and
;;; cloneable and permit both cloneable and done. See below.

(require racket/generic)
(require lang/posn)

(require "sstruct.rkt")
(require "cloneable.rkt")
(permit-cloneable)
(permit-done)

(require "type-predicates.rkt")

(provide (all-defined-out))

; +--------+---------------------------------------------------------
; | Points |
; +--------+

;;; (pt x y) -> pt?
;;;   x : real?
;;;   y : real?
;;; Make a point.
(sstruct pt (x y)
  #:transparent
  #:cloneable
  #:guard
  (lambda (x y type-name)
    (param-check! pt 1 real? x)
    (param-check! pt 2 real? y)
    (values x y))
  #:done)

;;; (distance pt1 pt2) -> real?
;;;   pt1 : pt?
;;;   pt2 : pt?
;;; Find the distance between two points.
(define distance
  (lambda (pt1 pt2)
    (magnitude (make-rectangular (- (pt-x pt1) (pt-x pt2))
                                 (- (pt-y pt1) (pt-y pt2))))))

(define distance-alt
  (lambda (pt1 pt2)
    (sqrt (+ (sqr (- (pt-x pt1) (pt-x pt2)))
             (sqr (- (pt-y pt1) (pt-y pt2)))))))

;;; (angle-between-points a b c) -> real?
;;;   a : pt?
;;;   b : pt?
;;;   c : pt?
;;; Determine the angle between the lines a-b and b-c
(define angle-between-points
  (let ([r2d (/ 180 pi)])
    (lambda (a b c)
      (let* ([v1 (angle (make-rectangular (- (pt-x b) (pt-x a))
                                          (- (pt-y b) (pt-y a))))]
             [v2 (angle (make-rectangular (- (pt-x c) (pt-x b))
                                          (- (pt-y c) (pt-y b))))]
             [ang (- v2 v1)]
             [result (if (>= ang 0) ang (+ ang pi))])
        (* r2d result)))))

;;; (pt->posn pt) -> posn?
;;;   pt : pt?
;;; Convert a point to a position.
;;;
;;; (Ah, the joy of naming conventions.)
(define pt->posn
  (lambda (pt)
    (make-posn (pt-x pt) (pt-y pt))))

;;; (posn->pt posn) -> pt?
;;;   p : posn?
;;; Convert a position to a point.
(define posn->pt
  (lambda (p)
    (pt (posn-x p) (posn-y p))))
