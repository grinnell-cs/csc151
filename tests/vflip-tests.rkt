#lang racket

(require "../image.rkt")
(require rackunit)

;;; Tests for `vflip`

(define blue-ellipse (solid-ellipse 60 30 "blue"))
(define vflipped-blue-ellipse (vflip blue-ellipse))
(define doubly-vflipped-blue-ellipse (vflip (vflip blue-ellipse)))
(define hvflipped-blue-ellipse (hflip (vflip blue-ellipse)))
(define vhflipped-blue-ellipse (vflip (hflip blue-ellipse)))

(test-true "a vflipped image is an image" 
           (image? vflipped-blue-ellipse))
(test-true "a vflipped image is a transformed image"
           (transformed? vflipped-blue-ellipse))
(test-true "a vflipped image is a flipped image"
           (flipped? vflipped-blue-ellipse))
(test-true "a vflipped image is a vflipped image"
           (vflipped? vflipped-blue-ellipse))
(test-true "a vflipped ellipse is still an ellipse"
           (ellipse? vflipped-blue-ellipse))
(test-equal? "a vflipped ellipse has the same color"
             (image-color vflipped-blue-ellipse)
             (image-color blue-ellipse))
(test-= "a vflipped ellipse has the same image width"
        (image-width vflipped-blue-ellipse)
        (image-width blue-ellipse)
        0.1)
(test-= "a vflipped ellipse has the same image height"
        (image-height vflipped-blue-ellipse)
        (image-height blue-ellipse)
        0.1)
(test-= "a vflipped ellipse has the same ellipse width"
        (ellipse-width vflipped-blue-ellipse)
        (ellipse-width blue-ellipse)
        0.1)
(test-= "a vflipped ellipse has the same ellipse height"
        (ellipse-height vflipped-blue-ellipse)
        (ellipse-height blue-ellipse)
        0.1)

(test-true "a doubly-vflipped image is an image" 
           (image? doubly-vflipped-blue-ellipse))
(test-true "a doubly-vflipped image is a transformed image"
           (transformed? doubly-vflipped-blue-ellipse))
(test-true "a doubly-vflipped image is a flipped image"
           (flipped? doubly-vflipped-blue-ellipse))
(test-true "a doubly-vflipped image is an doubly-vflipped image"
           (vflipped? doubly-vflipped-blue-ellipse))
(test-true "a doubly-vflipped ellipse is still an ellipse"
           (ellipse? doubly-vflipped-blue-ellipse))
(test-equal? "a doubly-vflipped ellipse has the same color"
             (image-color doubly-vflipped-blue-ellipse)
             (image-color blue-ellipse))
(test-= "a doubly-vflipped ellipse has the same image width"
        (image-width doubly-vflipped-blue-ellipse)
        (image-width blue-ellipse)
        0.1)
(test-= "a doubly-vflipped ellipse has the same image height"
        (image-height doubly-vflipped-blue-ellipse)
        (image-height blue-ellipse)
        0.1)
(test-= "a doubly-vflipped ellipse has the same ellipse width"
        (ellipse-width doubly-vflipped-blue-ellipse)
        (ellipse-width blue-ellipse)
        0.1)
(test-= "a doubly-vflipped ellipse has the same ellipse height"
        (ellipse-height doubly-vflipped-blue-ellipse)
        (ellipse-height blue-ellipse)
        0.1)

(test-true "an hvflipped image is an image" 
           (image? hvflipped-blue-ellipse))
(test-true "an hvflipped image is a transformed image"
           (transformed? hvflipped-blue-ellipse))
(test-true "an hvflipped image is a flipped image"
           (flipped? hvflipped-blue-ellipse))
(test-true "an hvflipped image is an hflipped image"
           (hflipped? hvflipped-blue-ellipse))
(test-true "an hvflipped ellipse is still an ellipse"
           (ellipse? hvflipped-blue-ellipse))
(test-equal? "an hvflipped ellipse has the same color"
             (image-color hvflipped-blue-ellipse)
             (image-color blue-ellipse))
(test-= "an hvflipped ellipse has the same image width"
        (image-width hvflipped-blue-ellipse)
        (image-width blue-ellipse)
        0.1)
(test-= "an hvflipped ellipse has the same image height"
        (image-height hvflipped-blue-ellipse)
        (image-height blue-ellipse)
        0.1)
(test-= "an hvflipped ellipse has the same ellipse width"
        (ellipse-width hvflipped-blue-ellipse)
        (ellipse-width blue-ellipse)
        0.1)
(test-= "an hvflipped ellipse has the same ellipse height"
        (ellipse-height hvflipped-blue-ellipse)
        (ellipse-height blue-ellipse)
        0.1)

(test-true "a vhflipped image is an image" 
           (image? vhflipped-blue-ellipse))
(test-true "a vhflipped image is a transformed image"
           (transformed? vhflipped-blue-ellipse))
(test-true "a vhflipped image is a flipped image"
           (flipped? vhflipped-blue-ellipse))
(test-true "a vhflipped image is a vflipped image"
           (vflipped? vhflipped-blue-ellipse))
(test-true "a vhflipped ellipse is still an ellipse"
           (ellipse? vhflipped-blue-ellipse))
(test-equal? "a vhflipped ellipse has the same color"
             (image-color vhflipped-blue-ellipse)
             (image-color blue-ellipse))
(test-= "a vhflipped ellipse has the same image width"
        (image-width vhflipped-blue-ellipse)
        (image-width blue-ellipse)
        0.1)
(test-= "a vhflipped ellipse has the same image height"
        (image-height vhflipped-blue-ellipse)
        (image-height blue-ellipse)
        0.1)
(test-= "a vhflipped ellipse has the same ellipse width"
        (ellipse-width vhflipped-blue-ellipse)
        (ellipse-width blue-ellipse)
        0.1)
(test-= "a vhflipped ellipse has the same ellipse height"
        (ellipse-height vhflipped-blue-ellipse)
        (ellipse-height blue-ellipse)
        0.1)

