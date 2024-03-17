#lang racket

;;; File:
;;;   transformations.rkt
;;; Summary:
;;;   A variety of procedures for transforming images.
;;; Author:
;;;   Samuel A. Rebelsky
;;;
;;; IMPORTANT: If you are going to edit this file (or any of the
;;; imported files), make sure that you've updated the DrRacket editor
;;; to treat `sstruct` like `struct`.

(require racket/generic)
(require racket/include)

(require (prefix-in 2htdp: 2htdp/image))

(require "sstruct.rkt")
(require "cloneable.rkt")
(permit-cloneable)
(permit-done)

(require "type-predicates.rkt")

(require "colors.rkt")
(require "point.rkt")
(require "line.rkt")
(require "image-core.rkt")

(provide (all-defined-out))

; +-----------+------------------------------------------------------
; | Rotations |
; +-----------+

(sstruct %rotated %transformed (angle)
  #:cloneable
  #:methods gen:preserved []
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "~a, rotated by ~a degrees"
               (image-description (subimage img))
               (rotated-angle img))))]
  #:methods gen:img-make-pict
  [(define image-make-pict
     (lambda (img)
       (trim (2htdp:rotate (rotated-angle img)
                           (image-picture (subimage img))))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'rotate
             (image-structure (subimage img))
             (rotated-angle img))))]
  #:done)

(define rotated? %rotated?)

;;; (rotated-angle rot) -> real?
;;;   rot : rotated?
;;; Determine the angle the original image was rotated by
(define rotated-angle %rotated-angle)

;;; (rotate img angle [description]) -> image?
;;;   img : image?
;;;   angle : real?
;;;   description : string?
;;; Create a new image by rotating `img` by `angle` degrees
;;; counter-clockwise.
(define rotate
  (lambda (img angle [description #f])
    (when (not (image? img))
      (error 'rotate
             "expects an image as the first parameter, received ~a"
             img))
    (when (not (real? angle))
      (error 'rotate
             "expects a real number as the angle of rotation (2nd param), received ~a"
             angle))
    (when (and description (not (string? description)))
      (error 'rotate
             "expects a string as the description (parameter 3), received ~a"
             description))
    (%rotated description
              #f
              #f
              #f
              img
              angle)))

; +-------+----------------------------------------------------------
; | Flips |
; +-------+

(sstruct %flipped %transformed ()
  #:methods gen:preserved []
  #:cloneable)

;;; (flipped? img) -> boolean?
;;;   img : image?
;;; Determines if `img` was created by one of the flipping procedures.
(define flipped? %flipped?)

(sstruct %hflipped %flipped ()
  #:cloneable
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "~a, flipped horizontally"
               (image-description (subimage img)))))]
  #:methods gen:img-make-pict
  [(define image-make-pict
     (lambda (img)
       (2htdp:flip-horizontal (image-picture (subimage img)))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'hflip (image-structure (subimage img)))))]
  #:done)

;;; (hflipped? img) -> boolean?
;;;   img : image?
;;; Determine if `img` was created by horizontally flipping
;;; another image.
(define hflipped? %hflipped?)

;;; (hflip img [description]) -> image?
;;;   img : image?
;;;   description : string?
;;; Flip `img` horizontally.
(define hflip
  (lambda (img [description #f])
    (%hflipped description
               #f
               #f
               #f
               img)))

(sstruct %vflipped %flipped ()
  #:cloneable
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "~a, flipped vertically"
               (image-description (subimage img)))))]
  #:methods gen:img-make-pict
  [(define image-make-pict
     (lambda (img)
       (2htdp:flip-vertical (image-picture (subimage img)))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'vflip (image-structure (subimage img)))))]
  #:done)

;;; (vflipped? img) -> boolean?
;;;   img : image?
;;; Determine if `img` was created by vertically flipping
;;; another image.
(define vflipped? %vflipped?)

;;; (vflip img [description]) -> image?
;;;   img : image?
;;;   description : string?
;;; Flip `img` vertically.
(define vflip
  (lambda (img [description #f])
    (%vflipped description
               #f
               #f
               #f
               img)))

; +------------+-----------------------------------------------------
; | Redescribe |
; +------------+

(sstruct %redescribed %transformed ()
  #:cloneable
  #:methods gen:preserved []
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (image-description (subimage img))))]
  #:methods gen:img-make-pict
  [(define image-make-pict
     (lambda (img)
       (image-picture (subimage img))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'redescribe
             (image-structure (subimage img))
             (image-description img))))]
  #:methods gen:img-recolor
  [(define .image-recolor
     (lambda (img color)
       (image-recolor (subimage img) color)))]
  #:done)

;;; (redescribed? img) -> boolean?
;;;   img : image?
;;; Determine if `img` has been created by redescribing an extant image.
(define redescribed? %redescribed?)

;;; (redescribe img desc) -> image?
;;;   img : image?
;;;   desc : string?
;;; Create an equivalent version of `img` but with a new description.
(define redescribe
  (lambda (img desc)
    (%redescribed desc
                  #f
                  #f
                  #f
                  img)))

; +--------+---------------------------------------------------------
; | Scaled |
; +--------+

(sstruct %scaled %transformed (hfactor vfactor)
  #:cloneable
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (let ([hfactor (scaled-hfactor img)]
             [vfactor (scaled-vfactor img)]
             [subdesc (image-description (subimage img))])
         (cond
           [(equal? hfactor vfactor)
            (format "~a, scaled by ~a" subdesc hfactor)]
           [(equal? hfactor 1)
            (format "~a, scaled vertically by ~a" subdesc vfactor)]
           [(equal? vfactor 1)
            (format "~a, scaled horizontally by ~a" subdesc hfactor)]
           [else
            (format "~a, scaled horizontally by ~a and vertically by ~a"
                    subdesc hfactor vfactor)]))))]
  #:methods gen:img-make-pict
  [(define image-make-pict
     (lambda (img)
       (2htdp:scale/xy (scaled-hfactor img)
                       (scaled-vfactor img)
                       (image-picture (subimage img)))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (let ([hfactor (scaled-hfactor img)]
             [vfactor (scaled-vfactor img)]
             [substru (image-structure (subimage img))])
         (cond
           [(equal? hfactor vfactor)
            (list 'scale substru hfactor)]
           [(equal? hfactor 1)
            (list 'vscale substru vfactor)]
           [(equal? vfactor 1)
            (list 'hscale substru hfactor)]
           [else
            (list 'hvscale substru hfactor vfactor)]))))]
  #:done)

(define scaled? %scaled?)
(define scaled-hfactor %scaled-hfactor)
(define scaled-vfactor %scaled-vfactor)

;;; (scale img factor [description]) -> image?
;;;   img : image?
;;;   factor : real?
;;;   description : string?
;;; Scale an image by the specified factor.
(define scale
  (lambda (img factor [description #f])
    (%scaled description
             #f
             #f
             #f
             img
             factor
             factor)))

;;; (hscale img factor [description]) -> image?
;;;   img : image?
;;;   amt : real?
;;;   description : string?
;;; Scale an image horizontally by the specified factor.
(define hscale
  (lambda (img factor [description #f])
    (%scaled description
             #f
             #f
             #f
             img
             factor
             1)))

;;; (vscale img factor [description]) -> image?
;;;   img : image?
;;;   factor : real?
;;;   description : string?
;;; Scale an image vertically by the specified factors.
(define vscale
  (lambda (img factor [description #f])
    (%scaled description
             #f
             #f
             #f
             img
             1
             factor)))

;;; (hvscale img hfactor vfactor [description]) -> image?
;;;   img : image?
;;;   factor : real?
;;;   description : string?
;;; Scale an image vertically by the specified factors.
(define hvscale
  (lambda (img hfactor vfactor [description #f])
    (%scaled description
             #f
             #f
             #f
             img
             hfactor
             vfactor)))

; +---------+--------------------------------------------------------
; | Cropped |
; +---------+

(sstruct %cropped %transformed (left top width height)
  #:cloneable
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (let ([subdesc (image-description (subimage img))])
         (format "~a, cropped to a left edge of ~a, a top edge of ~a, a width of ~a, and a height of ~a"
                 subdesc
                 (%cropped-left img)
                 (%cropped-top img)
                 (%cropped-width img)
                 (%cropped-height img)))))]
  #:methods gen:img-make-pict
  [(define image-make-pict
     (lambda (img)
       (2htdp:crop (%cropped-left img)
                   (%cropped-top img)
                   (%cropped-width img)
                   (%cropped-height img)
                   (image-picture (subimage img)))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'crop
             (image-structure (subimage img))
             (%cropped-left img)
             (%cropped-top img)
             (%cropped-width img)
             (%cropped-height img))))]
  #:done)

;;; (cropped? img) -> boolean?
;;;   img : image?
;;; Determine if `img` is a cropped version of another image.
(define cropped? %cropped?)

;;; (crop img left top width height [description]) -> image?
;;;   img : image?
;;;   left : real?
;;;   top : real?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   description : string?
;;; Crop `img` with the left edge at `left`, the top edge at `top`,
;;; and the specified width and height.
(define crop
  (lambda (img left top width height [description #f])
    (when (not (image? img))
      (error 'crop
             "expects an image for the first parameter, received ~a"
             img))
    (when (not (real? left))
      (error 'crop
             "expects a real number for the second parameter (left), received ~a"
             left))
    (when (not (real? top))
      (error 'crop
             "expects a real number for the third parameter (top), received ~a"
             top))
    (when (not (real? width))
      (error 'crop
             "expects a real number for the fourth parameter (width), received ~a"
             width))
    (when (not (real? height))
      (error 'crop
             "expects a real number for the fifth parameter (height), received ~a"
             height))
    (%cropped description #f #f #f
              img 
              left top width height)))

; +---------+--------------------------------------------------------
; | Shifted |
; +---------+

(sstruct %hshifted %transformed (hoff)
  #:cloneable
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (let ([subdesc (image-description (subimage img))]
             [hoff (%hshifted-hoff img)])
         (if (< hoff 0)
             (format "~a, shifted left by ~a" subdesc (- hoff))
             (format "~a, shifted right by ~a" subdesc hoff)))))]
  #:methods gen:img-make-pict
  [(define image-make-pict
     (lambda (img)
       (let ([subimg (subimage img)]
             [hoff (%hshifted-hoff img)])
         (if (< hoff 0)
             (2htdp:crop (- hoff) 0 
                         (+ hoff (image-width subimg))
                         (image-height subimg)
                         (image-picture subimg))
             (2htdp:beside (2htdp:rectangle hoff 1 0 "white")
                           (image-picture subimg))))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'hshift
             (image-structure (subimage img))
             (%hshifted-hoff img))))]
  #:done)

(sstruct %vshifted %transformed (voff)
  #:cloneable
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (let ([subdesc (image-description (subimage img))]
             [voff (%vshifted-voff img)])
         (if (< voff 0)
             (format "~a, shifted up by ~a" subdesc (- voff))
             (format "~a, shifted dow by ~a" subdesc voff)))))]
  #:methods gen:img-make-pict
  [(define image-make-pict
     (lambda (img)
       (let ([subimg (subimage img)]
             [voff (%vshifted-voff img)])
         (if (< voff 0)
             (2htdp:crop 0 (- voff)
                         (image-width subimg)
                         (+ voff (image-height subimg))
                         (image-picture subimg))
             (2htdp:above (2htdp:rectangle 1 voff 0 "white")
                          (image-picture subimg))))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'vshift
             (image-structure (subimage img))
             (%vshifted-voff img))))]
  #:done)

;;; (shifted? img) -> boolean?
;;;   img : image?
;;; Determine if `img` is a shifted version of another image.
(define shifted? (any-of %hshifted? %vshifted?))

;;; (hshift img hoff [description]) -> image?
;;;   img : image?
;;;   hoff: real?
;;;   description : string?
;;; Shift `img` horizontally by `hoff`. If `hoff` is positive, shifts
;;; to the right. If `hoff` is negative, shifts left, cutting off the
;;; left edge (which is not recoverable).
(define hshift
  (lambda (img hoff [description #f])
    (param-check! hshift 1 image? img)
    (param-check! hshift 2 real? hoff)
    (%hshifted description #f #f #f img hoff)))

;;; (vshift img voff [description]) -> image?
;;;   img : image?
;;;   voff: real?
;;;   description : string?
;;; Shift `img` vertically by `voff`. If `voff` is positive, shifts
;;; down. If `voff` is negative, shifts up, cutting off the top edge 
;;; (which is not recoverable).
(define vshift
  (lambda (img voff [description #f])
    (param-check! vshift 1 image? img)
    (param-check! vshift 2 real? voff)
    (%vshifted description #f #f #f img voff)))

; +---------+--------------------------------------------------------
; | Framing |
; +---------+

(sstruct %framed %transformed ()
  #:cloneable
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (let ([subdesc (image-description (subimage img))])
         (format "~a with a black frame around it" subdesc))))]
  #:methods gen:img-make-pict
  [(define image-make-pict
     (lambda (img)
       (let ([subimg (subimage img)])
         (2htdp:frame (image-picture subimg)))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'frame
             (image-structure (subimage img)))))]
  #:done)

;;; (framed? img) -> boolean?
;;;   img : image?
;;; Determine if `img` was built with the `frame` command.
(define framed? %framed?)

;;; (frame img) -> image?
;;;   img : image?
;;; Add a frame around the image (generally for debugging).
(define frame
  (lambda (img [description #f])
    (param-check! frame 1 image? img)
    (%framed description #f #f #f img)))

; +---------+--------------------------------------------------------
; | Helpers |
; +---------+

;;; (trim img) -> 2htdp:image?
;;;   img -> 2htdp:image?
;;; Trim any transparent lines on the sides of the image.
;;;
;;; Used to handle issues discovered in my approach to `overlay`
;;; when combined with rotation.
(define trim
  (lambda (img)
    (let* ([pixels (list->vector (2htdp:image->color-list img))]
           [width (2htdp:image-width img)]
           [height (2htdp:image-height img)]
           [first-row (first-solid-row pixels 
                                       width height 0 
                                       0 width)]
           [last-row (last-solid-row pixels 
                                     width height 
                                     (- height 1) 
                                     0 width)]
           [first-col (first-solid-col pixels
                                       width height
                                       0
                                       (max first-row 0)
                                       (min last-row height))]
           [last-col (last-solid-col pixels
                                     width height
                                     (- width 1)
                                     (max first-row 0)
                                     (min last-row height))]
           [hoff (min first-col (- width last-col))]
           [voff (min first-row (- height last-row))])
      (2htdp:crop hoff
                  voff 
                  (- width hoff hoff)
                  (- height voff voff)
                  img))))

;;; (first-solid-col pixels width height col startcol endcol)
;;;   pixels : (vector-of rgb?)
;;;   width : positive-integer?
;;;   height : positive-integer?
;;;   col : (all-of nonnegative-integer? (less-than width))
;;;   startcol : (all-of nonnegative-integer? (less-than height))
;;;   endcol : (all-of nonegative-integer? (at-least startcol) (less-than (image-height img)))
;;; Find the first column with a non-transparent color, moving right
;;; stating with col.
(define first-solid-col
  (lambda (pixels width height col startrow endrow)
    ; (displayln (list 'first-solid-col 'pixels width height col startrow endrow))
    (if (and (< col width)
             (2htdp-col-transparent? pixels width height col startrow endrow))
        (first-solid-col pixels width height (+ col 1) startrow endrow)
        col)))

;;; (first-solid-row pixels width height row startcol endcol)
;;;   pixels : (vector-of rgb?)
;;;   width : positive-integer?
;;;   height : positive-integer?
;;;   row : (all-of nonnegative-integer? (less-than height))
;;;   startcol : (all-of nonnegative-integer? (less-than width))
;;;   endcol : (all-of nonegative-integer? (at-least startcol) width)
;;; Find the first row with a non-transparent color, moving down start at row.
(define first-solid-row
  (lambda (pixels width height row startcol endcol)
    (if (and (< row height)
             (2htdp-row-transparent? pixels width height row startcol endcol))
        (first-solid-row pixels width height (+ row 1) startcol endcol)
        row)))

;;; (last-solid-col pixels width height col startcol endcol)
;;;   pixels : (vector-of rgb?)
;;;   width : positive-integer?
;;;   height : positive-integer?
;;;   col : (all-of nonnegative-integer? (less-than width))
;;;   startcol : (all-of nonnegative-integer? (less-than height))
;;;   endcol : (all-of nonegative-integer? (at-least startcol) (less-than (image-height img)))
;;; Find the last column with a non-transparent color, moving left starting
;;; with col.
(define last-solid-col
  (lambda (pixels width height col startrow endrow)
    (if (and (>= col 0)
             (2htdp-col-transparent? pixels width height col startrow endrow))
        (last-solid-col pixels width height (- col 1) startrow endrow)
        col)))

;;; (last-solid-row pixels width height row startcol endcol)
;;;   pixels : (vector-of rgb?)
;;;   width : positive-integer?
;;;   height : positive-integer?
;;;   row : (all-of nonnegative-integer? (less-than height))
;;;   startcol : (all-of nonnegative-integer? (less-than width)
;;;   endcol : (all-of nonegative-integer? (at-least startcol) (less-than width))
;;; Find the last row with a non-transparent color, moving up, starting at row.
(define last-solid-row
  (lambda (pixels width height row startcol endcol)
    (if (and (>= row 0)
             (2htdp-row-transparent? pixels width height row startcol endcol))
        (last-solid-row pixels width height (- row 1) startcol endcol)
        row)))

;;; (2htdp-row-transparent? pixels width height row startcol endcol)
;;;   pixels : (vector-of rgb?)
;;;   width : positive-integer?
;;;   height : positive-integer?
;;;   row : (all-of nonnegative-integer? (less-than height))
;;;   startcol : (all-of nonnegative-integer? (less-than (image-width img)))
;;;   endcol : (all-of nonegative-integer? (at-least startcol) (less-than (image-width img)))
;;; Determine if the given row is transparent.
(define 2htdp-row-transparent?
  (lambda (pixels width height row startcol endcol)
    (let kernel ([col startcol]
                 [pos (+ startcol (* row width))])
      (or (>= col endcol)
          (and (zero? (2htdp:color-alpha (vector-ref pixels pos)))
               (kernel (+ col 1) (+ pos 1)))))))

;;; (2htdp-col-transparent? pixels width height col startrow endrow)
;;;   pixels : (vector-of rgb?)
;;;   width : positive-integer?
;;;   height : positive-integer?
;;;   col : (all-of nonnegative-integer? (less-than width))
;;;   startrow : (all-of nonnegative-integer? (less-than (image-height img)))
;;;   endrow : (all-of nonegative-integer? (at-least startrow) (less-than (image-height img)))
;;; Determine if the given row is transparent.
(define 2htdp-col-transparent?
  (lambda (pixels width height col startrow endrow)
    (let kernel ([row startrow]
                 [pos (+ col (* startrow width))])
      (or (>= row endrow)
          (and (zero? (2htdp:color-alpha (vector-ref pixels pos)))
               (kernel (+ row 1) (+ pos width)))))))

;;; (get-pixel pixels col row width height) -> rgb?
;;;   pixels : (vector-of rgb?)
;;;   col : (all-of nonnegative-integer? (less-than width))
;;;   row : (all-of nonnegative-integer? (less-than-height))
;;;   width : nonnegative-integer?
;;;   height : nonnegative-integer?
;;; Gets the given pixel from the pixel grid.
(define get-pixel
  (lambda (pixels col row width height)
    (vector-ref pixels (+ col (* row width)))))
