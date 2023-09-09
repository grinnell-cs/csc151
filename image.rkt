#lang racket

(require 2htdp/image)

;;; File:
;;;   image.rkt
;;; Summary:
;;;   A variety of procedures for working with images.
;;; Author:
;;;   Samuel A. Rebelsky

; (provide image-compute)
(provide image-map)
(provide image-load)
(provide image-save)

; +---------------------+--------------------------------------------
; | Exported procedures |
; +---------------------+

;;; (image-map proc img) -> image?
;;;   proc : procedure (color->color)
;;;   img : image?
;;; Create a new image by applying proc to every pixel in img.
(define image-map
  (lambda (proc img)
    (color-list->bitmap (map proc (image->color-list img))
                        (image-width img)
                        (image-height img))))


(define image-load
  (lambda (fname)
    (bitmap/file fname)))

(define image-save
  (lambda (img fname)
    (save-image img fname)))
