#lang racket

;;; File:
;;;   ellipse.rkt
;;; Summary:
;;;   A variety of procedures for working with ellipses.
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

; +----------+-------------------------------------------------------
; | Ellipses |
; +----------+

(sstruct %ellipse %shape (width height)
  #:cloneable)

;;; (ellipse-width ell) -> nonnegative-real?
;;;   ell : ellipse?
;;; Determine the width of an ellipse. For outlined ellipses, this is
;;; the width of the inner ellipse.
(define ellipse-width
  (lambda (img)
    (cond
      [(%ellipse? img)
       (%ellipse-width img)]
      [(transformed? img)
       (ellipse-width (subimage img))]
      [else
       (error 'ellipse-width "expected an ellipse, received ~e" img)])))

;;; (ellipse-height ell) -> nonnegative-real?
;;;   ell : ellipse?
;;; Determine the height of an ellipse. For outlined ellipses, this is
;;; the height of the inner ellipse.
(define ellipse-height
  (lambda (img)
    (cond
      [(%ellipse? img)
       (%ellipse-height img)]
      [(transformed? img)
       (ellipse-height (subimage img))]
      [else
       (error 'ellipse-height "expected an ellipse, received ~e" img)])))

;;; (ellipse? img) -> boolean?
;;;   img : image?
;;; Determine if an image is an ellipse.
(define ellipse?
  (lambda (img)
    (or (solid-ellipse? img)
        (outlined-ellipse? img)
        (and (transformed? img)
             (preserved? img)
             (ellipse? (subimage img))))))

; +----------------+-------------------------------------------------
; | Solid ellipses |
; +----------------+

(sstruct %solid-ellipse %ellipse ()
  #:cloneable
  #:methods gen:solid []
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/solid-~a-ellipse-~ax~a.png"
               (or dir ".")
               (color->color-name (image-color img))
               (image-width img)
               (image-height img))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "a solid ~a ~a-by-~a ellipse"
               (describe-color (image-color img))
               (ellipse-width img)
               (ellipse-height img))))]
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
       (list 'solid-ellipse
             (ellipse-width img)
             (ellipse-height img)
             (image-color img))))]
  #:done)

(define solid-ellipse? %solid-ellipse?)
(define solid-ellipse-width %ellipse-width)
(define solid-ellipse-height %ellipse-height)

;;; (solid-ellipse width height color [description]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   color : color?
;;;   description : string?
;;; A `width`-by-`height` ellipse whose color is `color`.
(define solid-ellipse
  (lambda (width
           height
           color
           [description #f])
    (param-check! solid-ellipse 1 nonnegative-real? width)
    (param-check! solid-ellipse 2 nonnegative-real? height)
    (param-check! solid-ellipse 3 color? color)
    (when description
      (param-check! solid-ellipse 4 string? description))
    (let ([color (color->rgb color)])
      (%solid-ellipse description
                      #f
                      #f
                      #f
                      color
                      width
                      height))))

(sstruct %outlined-ellipse %ellipse (line-width)
  #:cloneable
  #:methods gen:outlined []
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/outlined-~a-ellipse-~ax~a.png"
               (or dir ".")
               (color->color-name (image-color img))
               (image-width img)
               (image-height img))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "an outlined ~a ~a-by-~a ellipse"
               (describe-color (image-color img))
               (ellipse-width img)
               (ellipse-height img))))]
  #:methods gen:img-make-pict
  [(define image-make-pict
     (lambda (img)
       (let* ([lw (line-width img)]
              [tmp (2htdp:ellipse (+ (ellipse-width img) lw)
                                  (+ (ellipse-height img) lw)
                                  "outline"
                                  (2htdp:pen (color->2htdp (image-color img))
                                             lw
                                             "solid"
                                             "round"
                                             "miter"))])
         (2htdp:overlay tmp
                        (transparent-2htdp-rectangle (+ (ellipse-width img) lw lw)
                                                     (+ (ellipse-height img) lw lw))))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'outlined-ellipse
             (ellipse-width img)
             (ellipse-height img)
             (image-color img))))]
  #:methods gen:img-line-width
  [(define line-width
     (lambda (img)
       (max 0
            (min 255
                 (round (%outlined-ellipse-line-width img))))))]
  #:done)

; +-------------------+----------------------------------------------
; | Outlined ellipses |
; +-------------------+

(define outlined-ellipse? %outlined-ellipse?)
(define outlined-ellipse-width %ellipse-width)
(define outlined-ellipse-height %ellipse-height)

;;; (outlined-ellipse width height color line-width [description]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   color : color?
;;;   line-width : positive-integer?
;;;   description : string?
;;; A `width`-by-`height` ellipse whose color is `color`.
(define outlined-ellipse
  (lambda (width
           height
           color
           line-width
           [description #f])
    (param-check! outlined-ellipse 1 nonnegative-real? width)
    (param-check! outlined-ellipse 2 nonnegative-real? height)
    (param-check! outlined-ellipse 3 color? color)
    (param-check! outlined-ellipse 4 positive-integer? line-width)
    (when description
      (param-check! outlined-ellipse 5 string? description))
    (let ([color (color->rgb color)])
      (%outlined-ellipse description
                         #f
                         #f
                         #f
                         color
                         width
                         height
                         line-width))))

; +----------------------+-------------------------------------------
; | 2hdtp-style ellipses |
; +----------------------+

;;; (ellipse width height mode color-or-pen [description]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Create the described ellipse.
(define ellipse
  (lambda (width height mode color-or-pen [description #f])
    (2htdp-style 'ellipse
                 (lambda (color)
                   (solid-ellipse width height color description))
                 (lambda (color line-width)
                   (outlined-ellipse width height color line-width description))
                 mode
                 color-or-pen)))

