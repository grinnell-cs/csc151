#lang racket

;;; File:
;;;   triangle.rkt
;;; Summary:
;;;   A variety of procedures for working with triangles.
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

; +---------------------------+--------------------------------------
; | Solid isosceles triangles |
; +---------------------------+

(sstruct %solid-isosceles-triangle %solid-polygon (width height)
  #:cloneable
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/solid-~a-~ax~a-isosceles-triangle.png"
               (or dir ".")
               (if (precise-polygon-colors)
                   (color->hex (image-color img))
                   (color->color-name (image-color img)))
               (isosceles-triangle-width img)
               (isosceles-triangle-height img))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "a solid ~a ~a-by-~a isosceles triangle"
               (describe-color (image-color img))
               (isosceles-triangle-width img)
               (isosceles-triangle-height img))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'solid-isosceles-triangle
             (isosceles-triangle-width img)
             (isosceles-triangle-height img)
             (image-color img))))]
  #:done)

(define solid-isosceles-triangle? %solid-isosceles-triangle?)
(define solid-isosceles-triangle-width  %solid-isosceles-triangle-width)
(define solid-isosceles-triangle-height %solid-isosceles-triangle-height)

;;; (isosceles-triangle-points width height) -> (list-of pt?)
;;;   width : positive-real?
;;;   height : positive-real?
;;; Compute the points in an isosceles triangle of the given
;;; width and height.
(define isosceles-triangle-points
  (lambda (width height)
    (list (pt 0 height) (pt (/ width 2) 0) (pt width height))))

;;; (solid-isosceles-triangle width height color [desc]) -> image?
;;;   width : positive-real?
;;;   height : positive-real?
;;;   color : color?
;;;   description : string?
;;; A solid isosceles triangle of the given width, height, and color.
(define solid-isosceles-triangle
  (lambda (width
           height
           color
           [description #f])
    (param-check! solid-isosceles-triangle 1 nonnegative-real? width)
    (param-check! solid-isosceles-triangle 2 nonnegative-real? height)
    (param-check! solid-isosceles-triangle 3 color? color)
    (when description
      (param-check! solid-isosceles-triangle 4 string? description))
    (%solid-isosceles-triangle description
                               #f
                               #f
                               #f
                               (color->rgb color)
                               (isosceles-triangle-points width height)
                               width
                               height)))

(sstruct %outlined-isosceles-triangle %outlined-polygon (width height)
  #:cloneable
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/outlined-~a-~a-~ax~a-isosceles-triangle.png"
               (or dir ".")
               (color->color-name (image-color img))
               (line-width img)
               (isosceles-triangle-width img)
               (isosceles-triangle-height img))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "an outlined ~a ~a-by-~a isosceles-triangle"
               (describe-color (image-color img))
               (isosceles-triangle-width img)
               (isosceles-triangle-height img))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'outlined-isosceles-triangle
             (isosceles-triangle-width img)
             (isosceles-triangle-height img)
             (image-color img)
             (line-width img))))]
  #:done)

; +------------------------------+-----------------------------------
; | Outlined isosceles triangles |
; +------------------------------+

(define outlined-isosceles-triangle? %outlined-isosceles-triangle?)
(define outlined-isosceles-triangle-width %outlined-isosceles-triangle-width)
(define outlined-isosceles-triangle-height %outlined-isosceles-triangle-height)

;;; (outlined-isosceles-triangle width height color line-width [desc]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   color : color?
;;;   line-width : positive-integer?
;;;   description : string?
;;; An outlined isosceles-triangle whose inner size is width-by-height with
;;; an outline of `line-width`.
(define outlined-isosceles-triangle
  (lambda (width
           height
           color
           line-width
           [description #f])
    (param-check! outlined-isosceles-triangle 1 nonnegative-real? width)
    (param-check! outlined-isosceles-triangle 2 nonnegative-real? height)
    (param-check! outlined-isosceles-triangle 3 color? color)
    (param-check! outlined-isosceles-triangle 4 positive-integer? line-width)
    (when description
      (param-check! outlined-isosceles-triangle 5 string? description))
    (%outlined-isosceles-triangle description
                                  #f
                                  #f
                                  #f
                                  (color->rgb color)
                                  (isosceles-triangle-points width height)
                                  line-width
                                  width
                                  height)))

;;; (isosceles-triangle-polygon? poly) -> boolean?
;;;   poly : polygon?
;;; Determine if poly is a isosceles-triangle.
;;;
;;; Not currently implemented.
(define isosceles-triangle-polygon?
  (lambda (poly)
    #f))

;;; (isosceles-triangle? img) -> boolean?
;;;   img : image?
;;; Determine if `img` is an isosceles-triangle.
(define isosceles-triangle?
  (lambda (img)
    (or (solid-isosceles-triangle? img)
        (outlined-isosceles-triangle? img)
        (and (transformed? img)
             (preserved? img)
             (isosceles-triangle? (subimage img))))))

;;; (isosceles-triangle-height d) -> real?
;;;   tri : isosceles-triangle?
;;; Determine the height of an isosceles-triangle
(define isosceles-triangle-height
  (lambda (tri)
    (cond
      [(solid-isosceles-triangle? tri)
       (solid-isosceles-triangle-height tri)]
      [(outlined-isosceles-triangle? tri)
       (outlined-isosceles-triangle-height tri)]
      [(and (transformed? tri) (preserved? tri))
       (isosceles-triangle-height (subimage tri))]
      [else
       (error 'isosceles-triangle-height "not an isosceles-triangle ~a" tri)])))

;;; (isosceles-triangle-width tri) -> real?
;;;   tri : isosceles-triangle?
;;; Determine the width of an isosceles-triangle
(define isosceles-triangle-width
  (lambda (tri)
    (cond
      [(solid-isosceles-triangle? tri)
       (solid-isosceles-triangle-width tri)]
      [(outlined-isosceles-triangle? tri)
       (outlined-isosceles-triangle-width tri)]
      [(and (transformed? tri) (preserved? tri))
       (isosceles-triangle-width (subimage tri))]
      [else
       (error 'isosceles-triangle-width "not an isosceles-triangle ~a" tri)])))

; +---------------------------------+--------------------------------
; | 2htdp-style isosceles triangles |
; +---------------------------------+

;;; (isosceles-triangle width height mode color-or-pen [description]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Create the described polygon.
(define isosceles-triangle
  (lambda (width height mode color-or-pen [description #f])
    (2htdp-style 'isosceles-triangle
                 (lambda (color)
                   (solid-isosceles-triangle width height color description))
                 (lambda (color line-width)
                   (outlined-isosceles-triangle width height color line-width description))
                 mode
                 color-or-pen)))

; +-----------------------------+------------------------------------
; | Solid equilateral triangles |
; +-----------------------------+

(sstruct %solid-equilateral-triangle %solid-isosceles-triangle ()
  #:cloneable
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/solid-~a-~a-equilateral-triangle.png"
               (or dir ".")
               (if (precise-polygon-colors)
                   (color->hex (image-color img))
                   (color->color-name (image-color img)))
               (equilateral-triangle-edge img))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "a solid ~a equilateral triangle with edge length ~a"
               (describe-color (image-color img))
               (equilateral-triangle-edge img))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'solid-equilateral-triangle
             (equilateral-triangle-edge img)
             (image-color img))))]
  #:done)

;;; (solid-equilateral-triangle? val) -> boolean?
;;;   val : any?
;;; Determines if `val` is a solid equilateral triangle.
(define solid-equilateral-triangle? %solid-equilateral-triangle?)

;;; (solid-equilateral-triangle edge color [description]) -> image?
;;;   edge: positive-real?
;;;   color : color?
;;;   description : string?
;;; A solid equilateral triangle of the given edge length and color.
(define solid-equilateral-triangle
  (lambda (edge color [description #f])
    (param-check! solid-equilateral-triangle 1 nonnegative-real? edge)
    (param-check! solid-equilateral-triangle 2 color? color)
    (when description
      (param-check! solid-equilateral-triangle 3 string? description))
    (let ([width edge]
          [height (* sqrt-3 1/2 edge)])
      (%solid-equilateral-triangle description
                                   #f
                                   #f
                                   #f
                                   (color->rgb color)
                                   (isosceles-triangle-points width height)
                                   width
                                   height))))

(sstruct %outlined-equilateral-triangle %outlined-isosceles-triangle ()
  #:cloneable
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/outlined-~a-~a-~a-equilateral-triangle.png"
               (or dir ".")
               (color->color-name (image-color img))
               (line-width img)
               (equilateral-triangle-edge img))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "an outlined ~a equilateral-triangle with edge-length ~a"
               (describe-color (image-color img))
               (equilateral-triangle-edge img))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'outlined-equilateral-triangle
             (equilateral-triangle-edge img)
             (image-color img)
             (line-width img))))]
  #:done)

; +--------------------------------+---------------------------------
; | Outlined equilateral triangles |
; +--------------------------------+

;;; (outlined-equilateral-triangle? val) -> boolean?
;;;   val : any?
;;; Determine if `val` is an equilateral triangle.
(define outlined-equilateral-triangle? %outlined-equilateral-triangle?)

;;; (outlined-equilateral-triangle width height color line-width [desc]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   color : color?
;;;   line-width : positive-integer?
;;;   description : string?
;;; An outlined equilateral-triangle whose inner size is width-by-height with
;;; an outline of `line-width`. The inner triangle is transparent.
(define outlined-equilateral-triangle
  (lambda (edge color line-width [description #f])
    (param-check! outlined-equilateral-triangle 1 nonnegative-real? edge)
    (param-check! outlined-equilateral-triangle 2 color? color)
    (param-check! outlined-equilateral-triangle 3 positive-real? line-width)
    (when description
      (param-check! outlined-equilateral-triangle 4 string? description))
    (let ([width edge]
          [height (* sqrt-3 1/2 edge)])
      (%outlined-equilateral-triangle description
                                      #f
                                      #f
                                      #f
                                      (color->rgb color)
                                      (isosceles-triangle-points width height)
                                      line-width
                                      width
                                      height))))

;;; (equilateral-triangle-polygon? poly) -> boolean?
;;;   poly : polygon?
;;; Determine if poly is a equilateral-triangle.
;;;
;;; Not currently implemented.
(define equilateral-triangle-polygon?
  (lambda (poly)
    #f))

;;; (equilateral-triangle? img) -> boolean?
;;;   img : image?
;;; Determine if `img` is an equilateral-triangle.
(define equilateral-triangle?
  (lambda (img)
    (or (solid-equilateral-triangle? img)
        (outlined-equilateral-triangle? img)
        (and (transformed? img)
             (preserved? img)
             (equilateral-triangle? (subimage img))))))

;;; (equilateral-triangle-edge tri) -> real?
;;;   tri : equilateral-triangle?
;;; Determine the edge length of an equilateral-triangle
(define equilateral-triangle-edge
  (lambda (tri)
    (when (not (equilateral-triangle? tri))
      (error 'equilateral-triangle-edge
             "expects an equilateral triangle, received ~a"
             tri))
    (isosceles-triangle-width tri)))

; +-----------------------------------+------------------------------
; | 2hdtp-style equilateral triangles |
; +-----------------------------------+

;;; (equilateral-triangle edge mode color-or-pen [description]) -> image?
;;;   edge : nonnegative-real?
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Create the described triangle.
(define equilateral-triangle
  (lambda (edge mode color-or-pen [description #f])
    (2htdp-style 'equilateral-triangle
                 (lambda (color)
                   (solid-equilateral-triangle edge color description))
                 (lambda (color line-width)
                   (outlined-equilateral-triangle edge color line-width description))
                 mode
                 color-or-pen)))

; +-----------------------+--------------------------------------
; | Solid right triangles |
; +-----------------------+

(sstruct %solid-right-triangle %solid-polygon (width height)
  #:cloneable
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/solid-~a-~ax~a-right-triangle.png"
               (or dir ".")
               (if (precise-polygon-colors)
                   (color->hex (image-color img))
                   (color->color-name (image-color img)))
               (right-triangle-width img)
               (right-triangle-height img))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "a solid ~a ~a-by-~a right triangle"
               (describe-color (image-color img))
               (right-triangle-width img)
               (right-triangle-height img))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'solid-right-triangle
             (right-triangle-width img)
             (right-triangle-height img)
             (image-color img))))]
  #:done)

(define solid-right-triangle? %solid-right-triangle?)
(define solid-right-triangle-width  %solid-right-triangle-width)
(define solid-right-triangle-height %solid-right-triangle-height)

;;; (right-triangle-points width height) -> (list-of pt?)
;;;   width : positive-real?
;;;   height : positive-real?
;;; Compute the points in an right triangle of the given
;;; width and height.
(define right-triangle-points
  (lambda (width height)
    (list (pt 0 0) (pt width height) (pt 0 height))))

;;; (solid-right-triangle width height color [desc]) -> image?
;;;   width : positive-real?
;;;   height : positive-real?
;;;   color : color?
;;;   description : string?
;;; A solid right triangle of the given width, height, and color.
(define solid-right-triangle
  (lambda (width
           height
           color
           [description #f])
    (param-check! solid-right-triangle 1 nonnegative-real? width)
    (param-check! solid-right-triangle 2 nonnegative-real? height)
    (param-check! solid-right-triangle 3 color? color)
    (when description
      (param-check! solid-right-triangle 4 string? description))
    (%solid-right-triangle description
                           #f
                           #f
                           #f
                           (color->rgb color)
                           (right-triangle-points width height)
                           width
                           height)))

(sstruct %outlined-right-triangle %outlined-polygon (width height)
  #:cloneable
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/outlined-~a-~a-~ax~a-right-triangle.png"
               (or dir ".")
               (color->color-name (image-color img))
               (line-width img)
               (right-triangle-width img)
               (right-triangle-height img))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "an outlined ~a ~a-by-~a right-triangle"
               (describe-color (image-color img))
               (right-triangle-width img)
               (right-triangle-height img))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'outlined-right-triangle
             (right-triangle-width img)
             (right-triangle-height img)
             (image-color img)
             (line-width img))))]
  #:done)

; +--------------------------+---------------------------------------
; | Outlined right triangles |
; +--------------------------+

(define outlined-right-triangle? %outlined-right-triangle?)
(define outlined-right-triangle-width %outlined-right-triangle-width)
(define outlined-right-triangle-height %outlined-right-triangle-height)

;;; (outlined-right-triangle width height color line-width [desc]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   color : color?
;;;   line-width : positive-integer?
;;;   description : string?
;;; An outlined right-triangle whose inner size is width-by-height with
;;; an outline of `line-width`.
(define outlined-right-triangle
  (lambda (width
           height
           color
           line-width
           [description #f])
    (param-check! outlined-right-triangle 1 nonnegative-real? width)
    (param-check! outlined-right-triangle 2 nonnegative-real? height)
    (param-check! outlined-right-triangle 3 color? color)
    (param-check! outlined-right-triangle 4 positive-integer? line-width)
    (when description
      (param-check! outlined-right-triangle 5 string? description))
    (%outlined-right-triangle description
                              #f
                              #f
                              #f
                              (color->rgb color)
                              (right-triangle-points width height)
                              line-width
                              width
                              height)))

;;; (right-triangle-polygon? poly) -> boolean?
;;;   poly : polygon?
;;; Determine if poly is a right-triangle.
;;;
;;; Not currently implemented.
(define right-triangle-polygon?
  (lambda (poly)
    #f))

;;; (right-triangle? img) -> boolean?
;;;   img : image?
;;; Determine if `img` is an right-triangle.
(define right-triangle?
  (lambda (img)
    (or (solid-right-triangle? img)
        (outlined-right-triangle? img)
        (and (transformed? img)
             (preserved? img)
             (right-triangle? (subimage img))))))

;;; (right-triangle-height d) -> real?
;;;   tri : right-triangle?
;;; Determine the height of an right-triangle
(define right-triangle-height
  (lambda (tri)
    (cond
      [(solid-right-triangle? tri)
       (solid-right-triangle-height tri)]
      [(outlined-right-triangle? tri)
       (outlined-right-triangle-height tri)]
      [(and (transformed? tri) (preserved? tri))
       (right-triangle-height (subimage tri))]
      [else
       (error 'right-triangle-height "not an right-triangle ~a" tri)])))

;;; (right-triangle-width tri) -> real?
;;;   tri : right-triangle?
;;; Determine the width of an right-triangle
(define right-triangle-width
  (lambda (tri)
    (cond
      [(solid-right-triangle? tri)
       (solid-right-triangle-width tri)]
      [(outlined-right-triangle? tri)
       (outlined-right-triangle-width tri)]
      [(and (transformed? tri) (preserved? tri))
       (right-triangle-width (subimage tri))]
      [else
       (error 'right-triangle-width "not an right-triangle ~a" tri)])))

; +-----------------------------+---------------------------------
; | 2htdp-style right triangles |
; +-----------------------------+

;;; (right-triangle width height mode color-or-pen [description]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Create the described polygon.
(define right-triangle
  (lambda (width height mode color-or-pen [description #f])
    (2htdp-style 'right-triangle
                 (lambda (color)
                   (solid-right-triangle width height color description))
                 (lambda (color line-width)
                   (outlined-right-triangle width height color line-width description))
                 mode
                 color-or-pen)))

