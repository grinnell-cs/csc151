#lang racket

;;; File:
;;;   wedge.rkt
;;; Summary:
;;;   A variety of procedures for working with wedges of circles.
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

; +--------+---------------------------------------------------------
; | Wedges |
; +--------+

(sstruct %wedge %shape (radius angle)
  #:cloneable)

;;; (wedge-radius ell) -> nonnegative-real?
;;;   ell : wedge?
;;; Determine the radius of an wedge. For outlined wedges, this is
;;; the radius of the inner wedge.
(define wedge-radius
  (lambda (img)
    (cond
      [(%wedge? img)
       (%wedge-radius img)]
      [(transformed? img)
       (wedge-radius (subimage img))]
      [else
       (error 'wedge-radius "expected an wedge, received ~e" img)])))

;;; (wedge-angle ell) -> nonnegative-real?
;;;   ell : wedge?
;;; Determine the angle of an wedge. For outlined wedges, this is
;;; the angle of the inner wedge.
(define wedge-angle
  (lambda (img)
    (cond
      [(%wedge? img)
       (%wedge-angle img)]
      [(transformed? img)
       (wedge-angle (subimage img))]
      [else
       (error 'wedge-angle "expected an wedge, received ~e" img)])))

;;; (wedge? img) -> boolean?
;;;   img : image?
;;; Determine if an image is an wedge.
(define wedge?
  (lambda (img)
    (or (solid-wedge? img)
        (outlined-wedge? img)
        (and (transformed? img)
             (preserved? img)
             (wedge? (subimage img))))))

; +----------------+-------------------------------------------------
; | Solid wedges |
; +----------------+

(sstruct %solid-wedge %wedge ()
  #:cloneable
  #:methods gen:solid []
  #:methods gen:img-fname
  [(define .image-fname
     (lambda (img dir)
       (format "~a/solid-~a-wedge-~a~a.png"
               (or dir ".")
               (color->color-name (image-color img))
               (wedge-radius img)
               (wedge-angle img))))]
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "a solid ~a wedge of radius ~a and angle ~a"
               (describe-color (image-color img))
               (wedge-radius img)
               (wedge-angle img))))]
  #:methods gen:img-make-pict
  [(define image-make-pict
     (lambda (img)
       (2htdp:wedge (wedge-radius img)
                    (wedge-angle img)
                    "solid"
                    (color->2htdp (image-color img)))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'solid-wedge
             (wedge-radius img)
             (wedge-angle img)
             (image-color img))))]
  #:done)

(define solid-wedge? %solid-wedge?)
(define solid-wedge-radius %wedge-radius)
(define solid-wedge-angle %wedge-angle)

;;; (solid-wedge radius angle color [description]) -> image?
;;;   radius : nonnegative-real?
;;;   angle : real?
;;;   color : color?
;;;   description : string?
;;; A wedge with the given radius, angle, and color.
(define solid-wedge
  (lambda (radius
           angle
           color
           [description #f])
    (param-check! solid-wedge 1 positive-real? radius)
    (param-check! solid-wedge 2 real? angle)
    (param-check! solid-wedge 3 color? color)
    (when description
      (param-check! solid-wedge 4 string? description))
    (let ([color (color->rgb color)])
      (%solid-wedge description
                    #f
                    #f
                    #f
                    color
                    radius
                    angle))))

(sstruct %outlined-wedge %wedge (line-width)
  #:cloneable
  #:methods gen:outlined []
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "an outlined ~a wedge with radius ~a and angle ~a"
               (color->color-name (image-color img))
               (wedge-radius img)
               (wedge-angle img))))]
  #:methods gen:img-make-pict
  [(define image-make-pict
     (lambda (img)
       (let* ([lw (line-width img)]
              [radius (wedge-radius img)]
              [angle (wedge-angle img)]
              [new-radius (+ radius (* 2 (/ lw (sin (/ angle 2)))))]
              [tmp (2htdp:wedge radius angle
                                "outline"
                                (2htdp:pen (color->2htdp (image-color img))
                                           lw
                                           "solid"
                                           "round"
                                           "miter"))])
        tmp)))] ; TODO
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'outlined-wedge
             (wedge-radius img)
             (wedge-angle img)
             (image-color img))))]
  #:methods gen:img-line-width
  [(define line-width
     (lambda (img)
       (max 0
            (min 255
                 (round (%outlined-wedge-line-width img))))))]
  #:done)

; +-----------------+------------------------------------------------
; | Outlined wedges |
; +-----------------+

(define outlined-wedge? %outlined-wedge?)
(define outlined-wedge-radius %wedge-radius)
(define outlined-wedge-angle %wedge-angle)

;;; (outlined-wedge radius angle color line-width [description]) -> image?
;;;   radius : nonnegative-real?
;;;   angle : nonnegative-real?
;;;   color : color?
;;;   line-width : positive-integer?
;;;   description : string?
;;; A `radius`-by-`angle` wedge whose color is `color`.
(define outlined-wedge
  (lambda (radius
           angle
           color
           line-width
           [description #f])
    (param-check! outlined-wedge 1 positive-real? radius)
    (param-check! outlined-wedge 2 real? angle)
    (param-check! outlined-wedge 3 color? color)
    (param-check! outlined-wedge 4 positive-integer? line-width)
    (when description
      (param-check! outlined-wedge 5 string? description))
    (let ([color (color->rgb color)])
      (%outlined-wedge description
                         #f
                         #f
                         #f
                         color
                         radius
                         angle
                         line-width))))

; +----------------------+-------------------------------------------
; | 2hdtp-style wedges |
; +----------------------+

;;; (wedge radius angle mode color-or-pen [description]) -> image?
;;;   radius : positive-real?
;;;   angle : real?
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Create the described wedge.
(define wedge
  (lambda (radius angle mode color-or-pen [description #f])
    (2htdp-style 'wedge
                 (lambda (color)
                   (solid-wedge radius angle color description))
                 (lambda (color line-width)
                   (outlined-wedge radius angle color line-width description))
                 mode
                 color-or-pen)))

