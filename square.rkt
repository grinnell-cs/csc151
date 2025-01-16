#lang racket

;;; File:
;;;   square.rkt
;;; Summary:
;;;   A variety of procedures for working with squares.
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
(require "rectangle.rkt")

(provide (all-defined-out))

; +---------+--------------------------------------------------------
; | Squares |
; +---------+

(sstruct %solid-square %solid-rectangle ()
  #:cloneable
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/solid-~a-square-~a.png"
               (or dir ".")
               (color->color-name (image-color img))
               (square-side img))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "a solid ~a square with side length ~a"
               (describe-color (image-color img))
               (image-width img))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'solid-square (rectangle-width img) (image-color img))))]
  #:done)

(define solid-square? %solid-square?)

;;; (solid-square side color [description]) -> image?
;;;   side : nonnegative-integer?
;;;   color : color?
;;;   description : string?
;;; Create a solid square with the given side length and color.
(define solid-square
  (lambda (side color [description #f])
    (param-check! solid-square 1 nonnegative-real? side)
    (param-check! solid-square 2 color? color)
    (when description
      (param-check! solid-square 3 string? description))
    (let ([color (color->rgb color)]
          [points (list (pt 0 0) (pt side 0) (pt side side) (pt 0 side))])
      (%solid-square description
                     #f
                     #f
                     #f
                     color
                     points
                     side
                     side))))

(sstruct %outlined-square %outlined-rectangle ()
  #:cloneable
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/outlined-~a-~a-square-~a.png"
               (or dir ".")
               (line-width img)
               (color->color-name (image-color img))
               (square-side img))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "an outlined ~a square with side length ~a"
               (describe-color (image-color img))
               (image-width img))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'outlined-square (rectangle-width img) (image-color img))))]
  #:done)

(define outlined-square? %outlined-square?)

;;; (outlined-square side color line-width [description]) -> image?
;;;   side : nonnegative-integer?
;;;   color : color?
;;;   line-width : positive-real?
;;;   description : string?
;;; Create an outlined square with the given side length and color.
(define outlined-square
  (lambda (side color line-width [description #f])
    (param-check! outlined-square 1 nonnegative-real? side)
    (param-check! outlined-square 2 color? color)
    (param-check! outlined-square 3 positive-real? line-width)
    (when description
      (param-check! outlined-square 4 string? description))
    (let ([color (color->rgb color)]
          [points (list (pt 0 0) (pt side 0) (pt side side) (pt 0 side))])
      (%outlined-square description
                        #f
                        #f
                        #f
                        color
                        points
                        line-width
                        side
                        side))))

;;; (square? img) -> boolean?
;;;   img : image?
;;; Determine if the image is a square.
(define square?
  (lambda (img)
    (or (solid-square? img)
        (outlined-square? img)
        (and (rectangle? img)
             (<= (abs (- (rectangle-width img) (rectangle-height img))) 0.01))
        (and (transformed? img)
             (preserved? img)
             (square? (subimage img))))))

;;; (square-side sq) -> real?
;;;   sq : square?
;;; Determine the side length of a square.
(define square-side
  (lambda (sq)
    (rectangle-width sq)))

;;; (square edge mode color-or-pen [description]) -> image?
;;;   edge : nonnegative-real?
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Create the described square.
(define square
  (lambda (edge mode color-or-pen [description #f])
    (2htdp-style 'square
                 (lambda (color)
                   (solid-square edge color description))
                 (lambda (color line-width)
                   (outlined-square edge color line-width description))
                 mode
                 color-or-pen)))

