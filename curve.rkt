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

; +-----------------+------------------------------------------------
; | Standard curves |
; +-----------------+

(sstruct %curve %shape (source source-angle source-pull target target-angle target-pull line-width)
  #:reflection-name 'curve
  #:cloneable
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/curve-~a~a-from-~a-~a-angle~a-pull~a-to-~a,~a-angle~a-pull~a.png"
               (or dir ".")
               (round (%curve-line-width img))
               (color->color-name (image-color img))
               (pt-x (%curve-source img))
               (pt-y (%curve-source img))
               (round (%curve-source-angle img))
               (round (%curve-source-pull img))
               (pt-x (%curve-target img))
               (pt-y (%curve-target img))
               (round (%curve-target-angle img))
               (round (%curve-target-pull img)))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "a curved ~a line of width ~a connecting (~a,~a) to (~a,~a)"
               (color->color-name (image-color img))
               (round (%curve-line-width img))
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

(define curve-source %curve-source)
(define curve-source-angle %curve-source-angle)
(define curve-source-pull %curve-source-angle)
(define curve-target %curve-target)
(define curve-target-angle %curve-target-angle)
(define curve-target-pull %curve-target-angle)
(define curve-color image-color)
(define curve-line-width %curve-line-width)

;;; (curve source source-angle source-pull target target-angle target-pull color line-width [description])
;;;   source : pt?
;;;   source-angle : real?
;;;   source-pull : real?
;;;   target : pt?
;;;   target-angle : real?
;;;   target-pull : real?
;;;   color : color?
;;;   line-width : positive-integer?
;;;   description : string?
;;; Create a curve from `source` to `target`. The curve leaves
;;; `source` at the specified angle and enters `target` from the
;;; specified angle.
;;;
;;; (In both cases, 0 degrees is right, 90 degrees is up, 180 degrees
;;; is left, and 270 degrees is down.) The `pull` parameters indicate
;;; the inclination to curve in that direction; more pull means it
;;; travels further.
(define curve
  (lambda (source source-angle source-pull target target-angle
  target-pull color line-width [description #f])
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
	    color source source-angle source-pull target target-angle
	    target-pull line-width)))

; +---------------+--------------------------------------------------
; | Filled curves |
; +---------------+

(sstruct %filled-curve %curve ()
  #:reflection-name 'filled-curve
  #:cloneable
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/filled-curve-~a-from-~a-~a-angle~a-pull~a-to-~a,~a-angle~a-pull~a.png"
               (or dir ".")
               (color->color-name (image-color img))
               (pt-x (%curve-source img))
               (pt-y (%curve-source img))
               (round (%curve-source-angle img))
               (round (%curve-source-pull img))
               (pt-x (%curve-target img))
               (pt-y (%curve-target img))
               (round (%curve-target-angle img))
               (round (%curve-target-pull img)))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "a filled ~a curve connecting (~a,~a) to (~a,~a)"
               (color->color-name (image-color img))
               (pt-x (%curve-source img))
               (pt-y (%curve-source img))
               (pt-x (%curve-target img))
               (pt-y (%curve-target img)))))]
  #:methods gen:img-make-pict
  [(define image-make-pict
     (lambda (img)
       (2htdp:add-solid-curve (2htdp:square 0 "solid" (2htdp:color 255 255 255 0))
                              (pt-x (%curve-source img))
                              (pt-y (%curve-source img))
                              (%curve-source-angle img)
                              (%curve-source-pull img)
                              (pt-x (%curve-target img))
                              (pt-y (%curve-target img))
                              (+ 180 (%curve-target-angle img))
                              (%curve-target-pull img)
                              (image-color img))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'filled-curve
             (%curve-source img)
             (%curve-source-angle img)
             (%curve-source-pull img)
             (%curve-target img)
             (%curve-target-angle img)
             (%curve-target-pull img)
             (image-color img))))]
  #:done)

;;; (filled-curve source source-angle source-pull target target-angle target-pull color [description])
;;;   source : pt?
;;;   source-angle : real?
;;;   source-pull : real?
;;;   target : pt?
;;;   target-angle : real?
;;;   target-pull : real?
;;;   color : color?
;;;   description : string?
;;; Create a "filled" curve from `source` to `target`. The curve
;;; leaves `source` at the specified angle and enters `target` from
;;; the specified angle. The filled area is between the curve and
;;; the straight line between `source` and `target`.
;;;
;;; (In both cases, 0 degrees is right, 90 degrees is up, 180 degrees
;;; is left, and 270 degrees is down.) The `pull` parameters indicate
;;; the inclination to curve in that direction; more pull means it
;;; travels further.
(define filled-curve
  (lambda (source source-angle source-pull target target-angle
  target-pull color [description #f])
    (param-check! filled-curve 1 pt? source)
    (param-check! filled-curve 2 real?  source-angle)
    (param-check! filled-curve 3 real? source-pull)
    (param-check! filled-curve 4 pt? target)
    (param-check! filled-curve 5 real? target-angle)
    (param-check! filled-curve 6 real? target-pull)
    (param-check! filled-curve 7 color? color)
    (when description
      (param-check! filled-curve 8 string? description))
    (%filled-curve description #f #f #f
	           color source source-angle source-pull target target-angle
	           target-pull 0)))

