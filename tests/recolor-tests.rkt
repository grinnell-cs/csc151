#lang racket

(require "../image.rkt")
(require "../colors.rkt")
(require (prefix-in 2htdp: 2htdp/image))
(require rackunit)

;;; recolor-tests.rkt
;;;   Tests for `recolor`

(test-equal? "recoloring a square gives an image in the new color"
             (color->rgb (image-color (recolor (solid-square 10 "white") "blue")))
             (color->rgb "blue"))

(test-true "recoloring a square gives a square"
           (square? (recolor (solid-square 10 "white") "blue")))

(test-equal? "recoloring a square gives the right picture"
             (image-picture (recolor (solid-square 10 "white") "blue"))
             (2htdp:square 10 "solid" "blue"))

(test-true "recoloring an ellipse gives an ellipse"
           (ellipse? (recolor (solid-ellipse 10 5 "white") "blue")))

(test-equal? "recoloring a square gives the right structure"
             (image-structure (recolor (solid-square 10 "white") "orange"))
             (image-structure (solid-square 10 "orange")))

(test-equal? "recoloring a flipped square gives an image in the right color"
             (color->rgb 
              (image-color 
               (recolor 
                (hflip (solid-square 10 "white"))
                "yellow")))
             (color->rgb "yellow"))

(test-equal? "recoloring a rotated square gives an image in the right color"
             (color->rgb 
              (image-color 
               (recolor 
                (rotate
                 (solid-square 10 "white")
                 15)
                "blue")))
             (color->rgb "blue"))

(test-equal? "recoloring a scaled square gives an image in the right color"
             (color->rgb 
              (image-color 
               (recolor 
                (scale
                 (solid-square 10 "white")
                 3)
                "blue")))
             (color->rgb "blue"))

(test-equal? "recoloring a recolored square gives an image in the right color"
             (color->rgb 
              (image-color 
               (recolor 
                (recolor
                 (solid-square 10 "white")
                 "blue")
                "red")))
             (color->rgb "red"))

