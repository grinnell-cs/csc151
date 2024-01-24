#lang racket

(require "../image.rkt")
(require "../colors.rkt")
(require (prefix-in 2htdp: 2htdp/image))
(require rackunit)

;;; solid-diamond-tests.rkt
;;;   Tests for `solid-diamond`

(test-true "We can build a solid diamond"
           (and (solid-diamond 40 20 "blue") #t))

(test-true "Our diamond has a 2htdp/image"
           (2htdp:image? (image-picture 
                          (solid-diamond 40 20 (rgb 10 20 30)))))

(test-true "Our diamond has a description"
           (string? (image-description
                     (solid-diamond 40 20 (rgb 10 20 30)))))

(test-equal? "Our diamond has the right structure"
             (image-structure
              (solid-diamond 40 20 (rgb 10 20 30)))
             (list 'solid-diamond 40 20 (rgb 10 20 30)))

(test-equal? "Our diamond has the right image width"
             (image-width (solid-diamond 40 20 (rgb 10 20 30)))
             40)

(test-equal? "Our diamond has the right diamond width"
             (diamond-width (solid-diamond 40 20 (rgb 10 20 30)))
             40)

(test-equal? "Our diamond has the right image height"
             (image-height (solid-diamond 40 20 (rgb 10 20 30)))
             20)

(test-equal? "Our diamond has the right diamond height"
             (diamond-height (solid-diamond 40 20 (rgb 10 20 30)))
             20)


