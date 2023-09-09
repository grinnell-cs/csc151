#lang racket
(require 2htdp/image)
(require (except-in racket/draw make-color make-pen))

;;; File:
;;;   colors.rkt
;;; Author:
;;;   Samuel A. Rebelsky
;;; Summary:
;;;   Some simple utilities for dealing with colors.

(provide (all-defined-out))

;;; (rgb r g b) -> color?
;;;   r : integer?
;;;   g : integer?
;;;   b : integer?
;;; Create a new RGB color.  Values outside of bounds are capped
;;; appropriately.
(define rgb
  (let ([bound (lambda (x) (min 255 (max x 0)))])
    (lambda (r g b)
      (make-color (bound r) (bound g) (bound b)))))

;;; (rgba r g b a) -> color?
;;;   r : integer?
;;;   g : integer?
;;;   b : integer?
;;;   a : integer?
;;; Create a new RGBA color.  Values outside of bounds are capped
;;; appropriately.
(define rgba
  (let ([bound (lambda (x) (min 255 (max x 0)))])
    (lambda (r g b a)
      (make-color (bound r) (bound g) (bound b) (bound a)))))

;;; (color-name->rgb name) -> color? or false?
;;;    name : symbol? or string?
;;; Convert a color name to a color
(define color-name->rgb
  (lambda (name)
    (if (symbol? name)
        (color-name->rgb (symbol->string name))
        (let ([tmp (send the-color-database find-color name)])
          (color->rgb tmp)))))

;;; (color->rgb color) -> color? or false
;;;   color : color? (one of the many forms)
;;; Convert one of the many forms of colors to an RGB color
(define color->rgb
  (lambda (color)
    (cond
      [((is-a?/c color%) color)
       (make-color (send color red) (send color green) (send color blue))]
      [(not (image-color? color))
       #f]
      [(color? color)
       color]
      [(symbol? color)
       (color-name->rgb color)]
      [(string? color)
       (color-name->rgb color)]
      [else
       #f])))


;;; (red-component color) -> integer? or boolean?
;;;   color : image-color?
;;; Get the red component of a color.  Returns #f if it's not a color.
(define red-component
  (lambda (color)
    (cond
      [((is-a?/c color%) color)
       (send color red)]
      [(not (image-color? color))
       #f]
      [(color? color)
       (color-red color)]
      [(symbol? color)
       (red-component (send the-color-database find-color (symbol->string color)))]
      [(string? color)
       (red-component (send the-color-database find-color color))]
      [else
       #f])))

;;; (green-component color) -> integer? or boolean?
;;;   color : image-color?
;;; Get the green component of a color.  Returns #f if it's not a color.
(define green-component
  (lambda (color)
    (cond
      [((is-a?/c color%) color)
       (send color green)]
      [(not (image-color? color))
       #f]
      [(color? color)
       (color-green color)]
      [(symbol? color)
       (green-component (send the-color-database find-color (symbol->string color)))]
      [(string? color)
       (green-component (send the-color-database find-color color))]
      [else
       #f])))

;;; (blue-component color) -> integer? or boolean?
;;;   color : image-color?
;;; Get the blue component of a color.  Returns #f if it's not a color.
(define blue-component
  (lambda (color)
    (cond
      [((is-a?/c color%) color)
       (send color blue)]
      [(not (image-color? color))
       #f]
      [(color? color)
       (color-blue color)]
      [(symbol? color)
       (blue-component (send the-color-database find-color (symbol->string color)))]
      [(string? color)
       (blue-component (send the-color-database find-color color))]
      [else
       #f])))