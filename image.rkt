#lang racket

;;; File:
;;;   image.rkt
;;; Summary:
;;;   A variety of procedures for working with images.
;;; Author:
;;;   Samuel A. Rebelsky
;;;
;;; IMPORTANT: If you are going to edit this file (or any of the
;;; imported files), make sure that you've updated the DrRacket editor
;;; to treat `sstruct` like `struct`.

(require racket/generic)
(require racket/include)

(require (prefix-in 2htdp: 2htdp/image))

(require "sstruct.rkt")
(require "cloneable.rkt")
(permit-cloneable)
(permit-done)

(require "type-predicates.rkt")

(require "colors.rkt")
(require "point.rkt")
(require "line.rkt")

(require "image-core.rkt")
(require "image-text.rkt")
(require "polygon.rkt")
(require "rectangle.rkt")
(require "square.rkt")
(require "diamond.rkt")
(require "triangle.rkt")
(require "ellipse.rkt")
(require "circle.rkt")
(require "curve.rkt")
(require "wedge.rkt")
(require "transformations.rkt")
(require "combinations.rkt")

(provide 
  (all-from-out "image-core.rkt")
  (all-from-out "image-text.rkt")
  (all-from-out "polygon.rkt")
  (all-from-out "rectangle.rkt")
  (all-from-out "square.rkt")
  (all-from-out "diamond.rkt")
  (all-from-out "triangle.rkt")
  (all-from-out "ellipse.rkt")
  (all-from-out "circle.rkt")
  (all-from-out "curve.rkt")
  (all-from-out "wedge.rkt")
  (all-from-out "transformations.rkt")
  (all-from-out "combinations.rkt"))
