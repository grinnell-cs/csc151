#lang racket

(require "../image.rkt")
(require "../colors.rkt")
(require (prefix-in 2htdp: 2htdp/image))
(require rackunit)

;;; scale-tests.rkt
;;;   Tests for `scale`, `vscale`, `hscale`, and `hvscale`.

; +-------------+----------------------------------------------------
; | Preparation |
; +-------------+

(define bss (solid-square 100 "blue"))

; +-------+----------------------------------------------------------
; | Scale |
; +-------+

(test-true "scale by a factor of 2 gives an image"
           (image? (scale bss 2)))

(test-true "scale by a factor of 1/2 gives an image"
           (image? (scale bss 1/2)))

(test-equal? "scale by a factor of 2 gives the right width"
             (image-width (scale bss 2))
             200)

(test-equal? "scale by a factor of 2 gives the right height"
             (image-height (scale bss 2))
             200)

(test-equal? "scale by a factor of 1/2 gives the right width"
             (image-width (scale bss 1/2))
             50)

(test-equal? "scale by a factor of 1/2 gives the right height"
             (image-height (scale bss 1/2))
             50)

(test-true "scale by a factor of two has a description"
           (string? (image-description (scale bss 2))))

(test-true "scale by a factor of two has a structure"
           (list? (image-structure (scale bss 2))))

(test-equal? "scale by a factor of two has the right structure"
             (image-structure (scale bss 2))
             (list 'scale (image-structure bss) 2))

(test-true "scale by a factor of two has a 2htdp:image"
           (2htdp:image? (image-picture (scale bss 2))))

(test-equal? "scale by a factor of two has the right image"
             (image-picture (scale bss 2))
             (2htdp:scale 2 (image-picture bss)))

; +--------+---------------------------------------------------------
; | Hscale |
; +--------+

(test-true "hscale by a factor of 2 gives an image"
           (image? (hscale bss 2)))

(test-true "hscale by a factor of 1/2 gives an image"
           (image? (hscale bss 1/2)))

(test-equal? "hscale by a factor of 2 gives the right width"
             (image-width (hscale bss 2))
             200)

(test-equal? "hscale by a factor of 2 gives the right height"
             (image-height (hscale bss 2))
             100)

(test-equal? "hscale by a factor of 1/2 gives the right width"
             (image-width (hscale bss 1/2))
             50)

(test-equal? "hscale by a factor of 1/2 gives the right height"
             (image-height (hscale bss 1/2))
             100)

(test-true "hscale by a factor of two has a description"
           (string? (image-description (hscale bss 2))))

(test-true "hscale by a factor of two has a structure"
           (list? (image-structure (hscale bss 2))))

(test-equal? "hscale by a factor of two has the right structure"
             (image-structure (hscale bss 2))
             (list 'hscale (image-structure bss) 2))

(test-true "hscale by a factor of two has a 2htdp:image"
           (2htdp:image? (image-picture (hscale bss 2))))

(test-equal? "hscale by a factor of two has the right image"
             (image-picture (hscale bss 2))
             (2htdp:scale/xy 2 1 (image-picture bss)))

; +--------+---------------------------------------------------------
; | Vscale |
; +--------+

(test-true "vscale by a factor of 2 gives an image"
           (image? (vscale bss 2)))

(test-true "vscale by a factor of 1/2 gives an image"
           (image? (vscale bss 1/2)))

(test-equal? "vscale by a factor of 2 gives the right width"
             (image-width (vscale bss 2))
             100)

(test-equal? "vscale by a factor of 2 gives the right height"
             (image-height (vscale bss 2))
             200)

(test-equal? "vscale by a factor of 1/2 gives the right width"
             (image-width (vscale bss 1/2))
             100)

(test-equal? "vscale by a factor of 1/2 gives the right height"
             (image-height (vscale bss 1/2))
             50)

(test-true "vscale by a factor of two has a description"
           (string? (image-description (vscale bss 2))))

(test-true "vscale by a factor of two has a structure"
           (list? (image-structure (vscale bss 2))))

(test-equal? "vscale by a factor of two has the right structure"
             (image-structure (vscale bss 2))
             (list 'vscale (image-structure bss) 2))

(test-true "vscale by a factor of two has a 2htdp:image"
           (2htdp:image? (image-picture (vscale bss 2))))

(test-equal? "vscale by a factor of two has the right image"
             (image-picture (vscale bss 2))
             (2htdp:scale/xy 1 2 (image-picture bss)))

; +---------+--------------------------------------------------------
; | HVscale |
; +---------+

(test-true "hvscale by factors of 2 and 1/2 gives an image"
           (image? (hvscale bss 2 1/2)))

(test-true "hvscale by a factors of 1/2 and 2 gives an image"
           (image? (hvscale bss 1/2 2)))

(test-equal? "hvscale by factor of 2 and 1/2 gives the right width"
             (image-width (hvscale bss 2 1/2))
             200)

(test-equal? "hvscale by a factors of 2 and 1/2 gives the right height"
             (image-height (hvscale bss 2 1/2))
             50)

(test-equal? "hvscale by factors of 1/2 and 2 gives the right width"
             (image-width (hvscale bss 1/2 2))
             50)

(test-equal? "hvscale by a factors of 1/2 and 2 gives the right height"
             (image-height (hvscale bss 1/2 2))
             200)

(test-true "hvscale by factors of 2 and 1/2 has a description"
           (string? (image-description (hvscale bss 2 1/2))))

(test-true "hvscale by a factors of 2 and 1/2 has a structure"
           (list? (image-structure (hvscale bss 2 1/2))))

(test-equal? "hvscale by a factor of 2 and 1/2 has the right structure"
             (image-structure (hvscale bss 2 1/2))
             (list 'hvscale (image-structure bss) 2 1/2))

(test-true "hvscale by factors of 2 and 1/2 has a 2htdp:image"
           (2htdp:image? (image-picture (hvscale bss 2 1/2))))

(test-equal? "hvscale by factors of 2 and 1/2 has the right image"
             (image-picture (hvscale bss 2 1/2))
             (2htdp:scale/xy 2 1/2 (image-picture bss)))

(test-equal? "hvscale by factors of 2 and 2 is structured as scaling by 2"
             (image-structure (hvscale bss 2 2))
             (list 'scale (image-structure bss) 2))

(test-equal? "hvscale by factors of 2 and 1 is structured as hscaling by 2"
             (image-structure (hvscale bss 2 1))
             (list 'hscale (image-structure bss) 2))

(test-equal? "hvscale by factors of 1 and 2 is structured as vscaling by 2"
             (image-structure (hvscale bss 1 2))
             (list 'vscale (image-structure bss) 2))


