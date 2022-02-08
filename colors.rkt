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