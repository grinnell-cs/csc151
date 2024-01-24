#lang racket

(require "../image.rkt")
(require rackunit)

;;; Tests for `hflip`

(define blue-ellipse (solid-ellipse 60 30 "blue"))
(define hflipped-blue-ellipse (hflip blue-ellipse))
(define doubly-hflipped-blue-ellipse (hflip (hflip blue-ellipse)))

(test-true "an hflipped image is an image" 
           (image? hflipped-blue-ellipse))
(test-true "an hflipped image is a transformed image"
           (transformed? hflipped-blue-ellipse))
(test-true "an hflipped image is a flipped image"
           (flipped? hflipped-blue-ellipse))
(test-true "an hflipped image is an hflipped image"
           (hflipped? hflipped-blue-ellipse))
(test-true "an hflipped ellipse is still an ellipse"
           (ellipse? hflipped-blue-ellipse))
(test-equal? "an hflipped ellipse has the same color"
             (image-color hflipped-blue-ellipse)
             (image-color blue-ellipse))
(test-= "an hflipped ellipse has the same image width"
        (image-width hflipped-blue-ellipse)
        (image-width blue-ellipse)
        0.1)
(test-= "an hflipped ellipse has the same image height"
        (image-height hflipped-blue-ellipse)
        (image-height blue-ellipse)
        0.1)
(test-= "an hflipped ellipse has the same ellipse width"
        (ellipse-width hflipped-blue-ellipse)
        (ellipse-width blue-ellipse)
        0.1)
(test-= "an hflipped ellipse has the same ellipse height"
        (ellipse-height hflipped-blue-ellipse)
        (ellipse-height blue-ellipse)
        0.1)

(test-true "a doubly-hflipped image is an image" 
           (image? doubly-hflipped-blue-ellipse))
(test-true "a doubly-hflipped image is a transformed image"
           (transformed? doubly-hflipped-blue-ellipse))
(test-true "a doubly-hflipped image is a flipped image"
           (flipped? doubly-hflipped-blue-ellipse))
(test-true "a doubly-hflipped image is an doubly-hflipped image"
           (hflipped? doubly-hflipped-blue-ellipse))
(test-true "a doubly-hflipped ellipse is still an ellipse"
           (ellipse? doubly-hflipped-blue-ellipse))
(test-equal? "a doubly-hflipped ellipse has the same color"
             (image-color doubly-hflipped-blue-ellipse)
             (image-color blue-ellipse))
(test-= "a doubly-hflipped ellipse has the same image width"
        (image-width doubly-hflipped-blue-ellipse)
        (image-width blue-ellipse)
        0.1)
(test-= "a doubly-hflipped ellipse has the same image height"
        (image-height doubly-hflipped-blue-ellipse)
        (image-height blue-ellipse)
        0.1)
(test-= "a doubly-hflipped ellipse has the same ellipse width"
        (ellipse-width doubly-hflipped-blue-ellipse)
        (ellipse-width blue-ellipse)
        0.1)
(test-= "a doubly-hflipped ellipse has the same ellipse height"
        (ellipse-height doubly-hflipped-blue-ellipse)
        (ellipse-height blue-ellipse)
        0.1)

