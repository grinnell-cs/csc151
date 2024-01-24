#lang racket

(require "../image.rkt")
(require "../colors.rkt")
(require (prefix-in 2htdp: 2htdp/image))
(require rackunit)

;;; solid-rectangle-tests.rkt
;;;   Tests for `solid-rectangle`

(test-true "We can build a solid rectangle"
           (and (solid-rectangle 40 20 "blue") #t))

(test-true "Our rectangle has a 2htdp/image"
           (2htdp:image? (image-picture 
                          (solid-rectangle 40 20 (rgb 10 20 30)))))

(test-true "Our rectangle has a description"
           (string? (image-description
                     (solid-rectangle 40 20 (rgb 10 20 30)))))

(test-equal? "Our rectangle has the right structure"
             (image-structure
              (solid-rectangle 40 20 (rgb 10 20 30)))
             (list 'solid-rectangle 40 20 (rgb 10 20 30)))

(test-equal? "Our rectangle has the right image width"
             (image-width (solid-rectangle 40 20 (rgb 10 20 30)))
             40)

(test-equal? "Our rectangle has the right rectangle width"
             (rectangle-width (solid-rectangle 40 20 (rgb 10 20 30)))
             40)

(test-equal? "Our rectangle has the right image height"
             (image-height (solid-rectangle 40 20 (rgb 10 20 30)))
             20)

(test-equal? "Our rectangle has the right rectangle height"
             (rectangle-height (solid-rectangle 40 20 (rgb 10 20 30)))
             20)



