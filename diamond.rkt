#lang racket

;;; File:
;;;   diamond.rkt
;;; Summary:
;;;   A variety of procedures for working with diamonds.
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
(require "polygon.rkt")

(provide (all-defined-out))

; +----------------+-------------------------------------------------
; | Solid diamonds |
; +----------------+

(sstruct %solid-diamond %solid-polygon (width height)
  #:cloneable
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/solid-~a-~ax~a-diamond.png"
               (or dir ".")
               (color->color-name (image-color img))
               (diamond-width img)
               (diamond-height img))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "a solid ~a ~a-by-~a diamond"
               (color->color-name (image-color img))
               (diamond-width img)
               (diamond-height img))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'solid-diamond
             (diamond-width img)
             (diamond-height img)
             (image-color img))))]
  #:done)

(define solid-diamond? %solid-diamond?)
(define solid-diamond-width  %solid-diamond-width)
(define solid-diamond-height %solid-diamond-height)

;;; (solid-diamond width height color [desc]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   color : color?
;;;   description : string?
;;; A polygon whose vertices are given by `points` and whose color is `color`.
(define solid-diamond
  (lambda (width
           height
           color
           [description #f])
    (param-check! solid-diamond 1 nonnegative-real? width)
    (param-check! solid-diamond 2 nonnegative-real? height)
    (param-check! solid-diamond 3 color? color)
    (when description
      (param-check! solid-diamond 4 string? description))
    (%solid-diamond description
                    #f
                    #f
                    #f
                    (color->rgb color)
                    (diamond-points width height)
                    width
                    height)))

(sstruct %outlined-diamond %outlined-polygon (width height)
  #:cloneable
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/outlined-~a-~a-~ax~a-diamond.png"
               (or dir ".")
               (color->color-name (image-color img))
               (line-width img)
               (diamond-width img)
               (diamond-height img))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "an outlined ~a ~a-by-~a diamond"
               (color->color-name (image-color img))
               (diamond-width img)
               (diamond-height img))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'outlined-diamond
             (diamond-width img)
             (diamond-height img)
             (image-color img)
             (line-width img))))]
  #:done)

; +-------------------+----------------------------------------------
; | Outlined diamonds |
; +-------------------+

(define outlined-diamond? %outlined-diamond?)
(define outlined-diamond-width %outlined-diamond-width)
(define outlined-diamond-height %outlined-diamond-height)

;;; (outlined-diamond width height color line-width [desc]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   color : color?
;;;   line-width : positive-integer?
;;;   description : string?
;;; An outlined diamond whose inner size is width-by-height with
;;; an outlined of `line-width`.
(define outlined-diamond
  (lambda (width
           height
           color
           line-width
           [description #f])
    (param-check! outlined-diamond 1 nonnegative-real? width)
    (param-check! outlined-diamond 2 nonnegative-real? height)
    (param-check! outlined-diamond 3 color? color)
    (param-check! outlined-diamond 4 positive-integer? line-width)
    (when description
      (param-check! outlined-diamond 5 string? description))
    (%outlined-diamond description
                       #f
                       #f
                       #f
                       (color->rgb color)
                       (diamond-points width height)
                       line-width
                       width
                       height)))

;;; (diamond-polygon? poly) -> boolean?
;;;   poly : polygon?
;;; Determine if poly is a diamond.
(define diamond-polygon?
  (lambda (poly)
    (and (polygon? poly)
         (= 4 (polygon-sides poly))
         (= (polygon-side poly 0)
            (polygon-side poly 1)
            (polygon-side poly 2)
            (polygon-side poly 3)))))

;;; (diamond? img) -> boolean?
;;;   img : image?
;;; Determine if `img` is a diamond.
(define diamond?
  (lambda (img)
    (or (solid-diamond? img)
        (outlined-diamond? img)
        (diamond-polygon? img)
        (and (transformed? img)
             (preserved? img)
             (diamond? (subimage img))))))

;;; (diamond-height d) -> real?
;;;   d : diamond?
;;; Determine the height of a diamond
(define diamond-height
  (lambda (d)
    (cond
      [(solid-diamond? d)
       (solid-diamond-height d)]
      [(outlined-diamond? d)
       (outlined-diamond-height d)]
      [(diamond-polygon? d)
       (distance (polygon-point d 1) (polygon-point d 3))]
      [(and (transformed? d) (preserved? d))
       (diamond-height (subimage d))]
      [else
       (error 'diamond-height "not a diamond ~a" d)])))

;;; (diamond-width d) -> real?
;;;   d : diamond?
;;; Determine the width of a diamond
(define diamond-width
  (lambda (d)
    (cond
      [(solid-diamond? d)
       (solid-diamond-width d)]
      [(outlined-diamond? d)
       (outlined-diamond-width d)]
      [(diamond-polygon? d)
       (distance (polygon-point d 0) (polygon-point d 2))]
      [(and (transformed? d) (preserved? d))
       (diamond-width (subimage d))]
      [else
       (error 'diamond-width "not a diamond ~a" d)])))

;;; (diamond width height mode color-or-pen [description]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Create the described diamond.
(define diamond
  (lambda (width height mode color-or-pen [description #f])
    (2htdp-style 'diamond
                 (lambda (color)
                   (solid-diamond width height color description))
                 (lambda (color line-width)
                   (outlined-diamond width height color line-width description))
                 mode
                 color-or-pen)))

; +----------------------------------+-------------------------------
; | Miscellaneous diamond procedures |
; +----------------------------------+

;;; (diamond-points width height) -> (list-of point?)
;;;   width :positive-real?
;;;   height : positive-real?
;;; Determine the points for a width-by-height diamond.
(define diamond-points
  (lambda (width height)
    (let* ([w (/ width 2)]
           [h (/ height 2)])
      (list (pt w 0) (pt width h) (pt w height) (pt 0 h)))))

