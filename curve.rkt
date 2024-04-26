#lang racket

;;; File:
;;;   curve.rkt
;;; Summary:
;;;   A variety of procedures for working with curves.
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

(provide (all-defined-out))

; +-------------+----------------------------------------------------
; | Text basics |
; +-------------+

(sstruct %curve %shape (source source-angle source-pull target target-angle target-pull line-width)
  #:reflection-name 'curve
  #:cloneable
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "a curved line connecting (~a,~a) to (~a,~a)"
               (pt-x (%curve-source img))
               (pt-y (%curve-source img))
               (pt-x (%curve-target img))
               (pt-y (%curve-target img)))))]
  #:methods gen:img-make-pict
  [(define image-make-pict
     (lambda (img)
       (2htdp:add-curve (2htdp:square 0 "solid" (2htdp:color 255 255 255 0))
                        (pt-x (%curve-source img))
                        (pt-y (%curve-source img))
                        (%curve-source-angle img)
                        (%curve-source-pull img)
                        (pt-x (%curve-target img))
                        (pt-y (%curve-target img))
                        (+ 180 (%curve-target-angle img))
                        (%curve-target-pull img)
                        (2htdp:pen (image-color img)
                                   (%curve-line-width img)
                                   "solid"
                                   "round"
                                   "round"))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'curve
             (%curve-source img) 
             (%curve-source-angle img)
             (%curve-source-pull img)
             (%curve-target img)
             (%curve-target-angle img)
             (%curve-target-pull img)
             (%curve-line-width img))))]
  #:done)

(define curve
  (lambda (source source-angle source-pull target target-angle target-pull color line-width [description #f])
    (param-check! curve 1 pt? source)
    (param-check! curve 2 real? source-angle)
    (param-check! curve 3 real? source-pull)
    (param-check! curve 4 pt? target)
    (param-check! curve 5 real? target-angle)
    (param-check! curve 6 real? target-pull)
    (param-check! curve 7 color? color)
    (param-check! curve 8 positive-integer? line-width)
    (when description
      (param-check! curve 9 string? description))
    (%curve description #f #f #f
            color
            source
            source-angle
            source-pull
            target
            target-angle
            target-pull
            line-width)))
    