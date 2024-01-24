#lang racket

(require "../image.rkt")
(require "../colors.rkt")
(require (prefix-in 2htdp: 2htdp/image))
(require rackunit)

;;; solid-ellipse-tests.rkt
;;;   Tests for `solid-ellipse`

(test-true "We can build a solid ellipse"
           (and (solid-ellipse 40 20 "blue") #t))

(test-true "Our ellipse has a 2htdp/image"
           (2htdp:image? (image-picture 
                          (solid-ellipse 40 20 (rgb 10 20 30)))))

(test-equal? "Our ellipse is the same as the basic 2htdp/image ellipse"
             (image-picture (solid-ellipse 40 20 (rgb 10 20 30)))
             (2htdp:ellipse 40 20 "solid" (2htdp:color 10 20 30)))

(test-true "Our ellipse has a description"
           (string? (image-description
                     (solid-ellipse 40 20 (rgb 10 20 30)))))

(test-equal? "Our ellipse has the right structure"
             (image-structure
              (solid-ellipse 40 20 (rgb 10 20 30)))
             (list 'solid-ellipse 40 20 (rgb 10 20 30)))

(test-equal? "Our ellipse has the right image width"
             (image-width (solid-ellipse 40 20 (rgb 10 20 30)))
             40)

(test-equal? "Our ellipse has the right ellipse width"
             (ellipse-width (solid-ellipse 40 20 (rgb 10 20 30)))
             40)

(test-equal? "Our ellipse has the right image height"
             (image-height (solid-ellipse 40 20 (rgb 10 20 30)))
             20)

(test-equal? "Our ellipse has the right ellipse height"
             (ellipse-height (solid-ellipse 40 20 (rgb 10 20 30)))
             20)

