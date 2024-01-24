#lang racket

(require "../image.rkt")
(require "../colors.rkt")
(require (prefix-in 2htdp: 2htdp/image))
(require rackunit)

;;; solid-polygon-tests.rkt
;;;   Tests for `solid-polygon`

(define triangle-points
  (list (pt 0 0) (pt 100 0) (pt 0 40)))

(test-true "We can build a solid polygon"
           (and (solid-polygon triangle-points "blue") #t))

(test-true "Our polygon has a 2htdp/image"
           (2htdp:image? (image-picture 
                          (solid-polygon triangle-points "blue"))))

(test-true "Our polygon has a description"
           (string? (image-description
                     (solid-polygon triangle-points "blue"))))

(test-equal? "Our polygon has the right structure"
             (image-structure
              (solid-polygon triangle-points (rgb 0 0 0 0)))
             (list 'solid-polygon triangle-points (rgb 0 0 0 0)))

