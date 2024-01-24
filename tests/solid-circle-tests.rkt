#lang racket

(require "../image.rkt")
(require "../colors.rkt")
(require (prefix-in 2htdp: 2htdp/image))
(require rackunit)

;;; solid-circle-tests.rkt
;;;   Tests for `solid-circle`

(test-true "We can build a solid circle"
           (and (solid-circle 25 "blue") #t))

(test-true "Our circle has a 2htdp/image"
           (2htdp:image? (image-picture 
                          (solid-circle 25 (rgb 10 20 30)))))

#|
(test-equal? "Our circle has the right image"
             (image-picture (solid-circle 25 (rgb 10 20 30)))
             (2htdp:circle 25 "solid" (2htdp:color 10 20 30)))
|#

(test-true "Our circle has a description"
           (string? (image-description
                     (solid-circle 25 (rgb 10 20 30)))))

(test-equal? "Our circle has the right structure"
             (image-structure
              (solid-circle 25 (rgb 10 20 30)))
             (list 'solid-circle 25 (rgb 10 20 30)))

(test-equal? "Our circle has the right image width"
             (image-width (solid-circle 25 (rgb 10 20 30)))
             25)

(test-equal? "Our circle has the right diameter"
             (circle-diameter (solid-circle 25 (rgb 10 20 30)))
             25)
