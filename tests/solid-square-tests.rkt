#lang racket

(require "../image.rkt")
(require "../colors.rkt")
(require (prefix-in 2htdp: 2htdp/image))
(require rackunit)

;;; solid-square-tests.rkt
;;;   Tests for `solid-square`

(test-true "We can build a solid square"
           (and (solid-square 25 "blue") #t))

(test-true "Our square has a 2htdp/image"
           (2htdp:image? (image-picture 
                          (solid-square 25 (rgb 10 20 30)))))

(test-equal? "Our square has the right image"
             (image-picture (solid-square 25 (rgb 10 20 30)))
             (2htdp:square 25 "solid" (2htdp:color 10 20 30)))

(test-true "Our square has a description"
           (string? (image-description
                     (solid-square 25 (rgb 10 20 30)))))

(test-equal? "Our square has the right structure"
             (image-structure
              (solid-square 25 (rgb 10 20 30)))
             (list 'solid-square 25 (rgb 10 20 30)))

(test-equal? "Our square has the right image width"
             (image-width (solid-square 25 (rgb 10 20 30)))
             25)

(test-equal? "Our square has the right side length"
             (square-side (solid-square 25 (rgb 10 20 30)))
             25)
