#lang racket

;;; File:
;;;   circle.rkt
;;; Summary:
;;;   A variety of procedures for working with circles.
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
(require "ellipse.rkt")

(provide (all-defined-out))

; +---------+--------------------------------------------------------
; | Circles |
; +---------+

(sstruct %solid-circle %solid-ellipse ()
  #:cloneable
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/solid-~a-circle-~a.png"
               (or dir ".")
               (color->color-name (image-color img))
               (circle-diameter img))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "a solid ~a circle with diameter ~a"
               (describe-color (image-color img))
               (ellipse-width img))))]
  #:methods gen:img-make-pict
  [(define image-make-pict
     (lambda (img)
       (2htdp:ellipse (ellipse-width img)
                      (ellipse-height img)
                      "solid"
                      (color->2htdp (image-color img)))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'solid-circle (ellipse-width img) (image-color img))))]
  #:done)

(define solid-circle? %solid-circle?)

;;; (solid-circle diameter color [description]) -> image?
;;;   diameter : nonnegative-integer?
;;;   color : color?
;;;   description : string?
;;; Create a solid circle with the given diameter and color.
(define solid-circle
  (lambda (diameter color [description #f])
    (param-check! solid-circle 1 nonnegative-real? diameter)
    (param-check! solid-circle 2 color? color)
    (when description
      (param-check! solid-circle 3 string? description))
    (%solid-circle description
                   #f
                   #f
                   #f
                   color
                   diameter
                   diameter)))

(sstruct %outlined-circle %outlined-ellipse ()
  #:cloneable
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/outlined-~a-~a-circle-~a.png"
               (or dir ".")
               (line-width img)
               (color->color-name (image-color img))
               (circle-diameter img))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "an outlined ~a circle with diameter ~a"
               (describe-color (image-color img))
               (ellipse-width img))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'outlined-circle
             (ellipse-width img)
             (image-color img)
             (line-width img))))]
  #:done)

(define outlined-circle? %outlined-circle?)

;;; (outlined-circle diameter color [description]) -> image?
;;;   diameter : nonnegative-integer?
;;;   color : color?
;;;   line-width : positive-integer?
;;;   description : string?
;;; Create an outlined circle with the given diameter and color.
(define outlined-circle
  (lambda (diameter color line-width [description #f])
    (param-check! outlined-circle 1 nonnegative-real? diameter)
    (param-check! outlined-circle 2 color? color)
    (param-check! outlined-circle 3 positive-real? line-width)
    (when description
      (param-check! outlined-circle 4 string? description))
    (%outlined-circle description
                      #f
                      #f
                      #f
                      color
                      diameter
                      diameter
                      line-width)))

;;; (circle? img) -> boolean?
;;;   img : image?
;;; Determine if the image is a circle.
(define circle?
  (lambda (img)
    (or (solid-circle? img)
        (outlined-circle? img)
        (and (ellipse? img)
             (<= (abs (- (ellipse-width img) (ellipse-height img))) 0.01))
        (and (transformed? img)
             (preserved? img)
             (circle? (subimage img))))))

;;; (circle-diameter circ) -> real?
;;;   circ : circle?
;;; Determine the diameter of a circle.
(define circle-diameter
  (lambda (circ)
    (ellipse-width circ)))

;;; (circle diameter mode color-or-pen [description]) -> image?
;;;   diameter : nonnegative-real?
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Create the described circle.
(define circle
  (lambda (diameter mode color-or-pen [description #f])
    (2htdp-style 'circle
                 (lambda (color)
                   ; (displayln (format "Creating a ~a solid circle" color))
                   (solid-circle diameter color description))
                 (lambda (color line-width)
                   ; (displayln (format "Creating a ~a outlined circle" color))
                   (outlined-circle diameter color line-width description))
                 mode
                 color-or-pen)))

