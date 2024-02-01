#lang racket

;;; File:
;;;   image.rkt
;;; Summary:
;;;   A variety of procedures for working with images.
;;; Author:
;;;   Samuel A. Rebelsky

(require racket/generic)
(require (prefix-in 2htdp: 2htdp/image))
(require lang/posn)
(require "colors.rkt")
(require "sstruct.rkt")
(require "cloneable.rkt")
(require "type-predicates.rkt")

(provide (all-defined-out))

; +----------+-------------------------------------------------------
; | Settings |
; +----------+

;;; (markdown-dir [dir]) -> string?
;;;   dir : (any/of string? false?)
;;; Get or set the current markdown directory.
;;;
;;; The markdown directory is used for to detemrine how to output
;;; images. If it's not false, images get saved in that directory
;;; whenever they are displayed.
(define markdown-dir
  (let ([mdir #f])
    (lambda dir
      (when (not (null? dir))
        (set! mdir (car dir)))
      mdir)))

; +-----------+------------------------------------------------------
; | Constants |
; +-----------+

;;; sqrt-3 : real?
;;; The square root of three.
(define sqrt-3 (sqrt 3))

; +-------------+----------------------------------------------------
; | Struct tags |
; +-------------+

(set-sstruct-tag! #:done 0
                  (lambda (name)
                    null))

; Bleh. This is inelegant. It will have to suffice for now.
(set-sstruct-tag! #:cloneable 0
                  (lambda (name)
                    `(#:methods gen:cloneable
                      [(define clone
                         (lambda (val) (struct-copy ,name val)))])))

; +------------+-----------------------------------------------------
; | Interfaces |
; +------------+

;;; (image-make-bits img) -> (vector-of rgb?)
;;;   img : image?
;;; Make the bits for the image.
(define-generics img-make-bits
  (image-make-bits img-make-bits)
  #:fallbacks
  [(define image-make-bits
     (lambda (img)
       (list->vector (map 2htdp->rgb
                          (2htdp:image->color-list (image-picture img))))))])

;;; (image-make-desc img) -> string?
;;;   img : image?
;;; Make the desc of the image.
(define-generics img-make-desc
  (image-make-desc img-make-desc)
  #:fallbacks
  [(define image-make-desc
     (lambda (img)
       "an image"))])

;;; (image-make-pict img) -> 2htdp:image?
;;;   img : image?
;;; Make the picture associated with the image.
(define-generics img-make-pict
  (image-make-pict img-make-pict)
  #:fallbacks
  [(define image-make-pict
     (lambda (img)
       (2htdp:square 0 "solid" (2htdp:color 0 0 0 0))))])

;;; (image-make-stru img) -> list?
;;;   img : image?
;;; Make the overall structure of the image.
(define-generics img-make-stru
  (image-make-stru img-make-stru)
  #:fallbacks
  [(define image-make-stru
     (lambda (img)
       (list 'bitmap (image-width image) (image-height image))))])

;;; (image-color img) -> (any-of color? false?)
;;;   img : image?
;;; Get the color of this image. Returns false if its a type of
;;; image that does not have a natural color.
(define-generics img-color
  (.image-color img-color)
  #:fallbacks
  [(define .image-color
     (lambda (img)
       #f))])
(define image-color .image-color)

;;; (line-width img) -> integer?
;;;   img : image?
;;; Get the line width of the image. Returns 0 if it is not an outline or line.
(define-generics img-line-width
  (line-width img-line-width)
  #:fallbacks
  [(define line-width
     (lambda (img)
       0))])

;;; (image-recolor img color) -> image?
;;;   img : image?
;;;   color : color?
;;; Recolor an image.
(define-generics img-recolor
  (.image-recolor img-recolor color)
  #:fallbacks
  [(define .image-recolor
     (lambda (img color)
       img))])
(define image-recolor
  (lambda (img color)
    (.image-recolor img (color->rgb color))))
(define recolor image-recolor)

;;; (subimages img) -> (list-of image?)
;;;   img : image?
;;; Get a list of all the subimages of the current image.
(define-generics img-subimages
  (subimages img-subimages)
  #:fallbacks
  [(define subimages
     (lambda (img)
       null))])

;;; (image-fname img) -> string?
;;;   img : image?
;;;   dir : string?
;;; Generate a filename for the given image.
(define-generics img-fname
  (.image-fname img-fname dir)
  #:fallbacks
  [(define .image-fname
     (lambda (img dir)
       (make-image-fname dir "image")))])
(define image-fname .image-fname)

;;; (outlined? img) -> boolean?
;;;   img : image?
;;; Determine if an image is outlined.
(define-generics outlined)

;;; (solid? img) -> boolean?
;;;   img : image?
;;; Determine if an image is solid.
(define-generics solid)

;;; (preserved? t) -> boolean?
;;;   t : transformed?
;;; Determine if a transformed image preserved the underlying image.
(define-generics preserved)

; +--------+---------------------------------------------------------
; | Points |
; +--------+

;;; (pt x y) -> pt?
;;;   x : real?
;;;   y : real?
;;; Make a point.
(sstruct pt (x y)
         #:transparent
         #:cloneable)

;;; (distance pt1 pt2) -> real?
;;;   pt1 : pt?
;;;   pt2 : pt?
;;; Find the distance between two points.
(define distance
  (lambda (pt1 pt2)
    (magnitude (make-rectangular (- (pt-x pt1) (pt-x pt2))
                                 (- (pt-y pt1) (pt-y pt2))))))

(define distance-alt
  (lambda (pt1 pt2)
    (sqrt (+ (sqr (- (pt-x pt1) (pt-x pt2)))
             (sqr (- (pt-y pt1) (pt-y pt2)))))))

;;; (angle-between a b c) -> real?
;;;   a : pt?
;;;   b : pt?
;;;   c : pt?
;;; Determine the angle between the lines a-b and b-c
(define angle-between-points
  (let ([r2d (/ 180 pi)])
    (lambda (a b c)
      (let* ([v1 (angle (make-rectangular (- (pt-x b) (pt-x a))
                                          (- (pt-y b) (pt-y a))))]
             [v2 (angle (make-rectangular (- (pt-x c) (pt-x b))
                                          (- (pt-y c) (pt-y b))))]
             [ang (- v2 v1)]
             [result (if (>= ang 0) ang (+ ang pi))])
        (* r2d result)))))

;;; (pt->posn pt) -> posn?
;;;   pt : pt?
;;; Convert a point to a position.
;;;
;;; (Ah, the joy of naming conventions.)
(define pt->posn
  (lambda (pt)
    (make-posn (pt-x pt) (pt-y pt))))

; +----------------------+-------------------------------------------
; | Lines (mathematical) |
; +----------------------+

; ax + b
(struct linear (slope y-intercept) #:transparent)

;;; (linear-apply line x) -> real?
;;;   line : linear?
;;;   x : real?
;;; Apply a linear function to an x value.
(define linear-apply
  (lambda (line x)
    (+ (* (linear-slope line) x)
       (linear-y-intercept line))))

;;; (line-between pt1 pt2) -> (or linear? real?)
;;;   pt1 : point?
;;;   pt2 : point?
;;; Compute the function of the line between pt1 and pt2. Returns
;;; the x intercept for vertical lines
(define line-between
  (lambda (pt1 pt2)
    (let ([hoff (- (pt-x pt2) (pt-x pt1))]
          [voff (- (pt-y pt2) (pt-y pt1))])
      (if (zero? hoff)
          (pt-x pt1)
          (let* ([slope (/ voff hoff)]
                 [y-intercept (- (pt-y pt1) (* slope (pt-x pt1)))])
            (linear slope y-intercept))))))

;;; (offset-line line distance updown) -> linear?
;;;   line : (any-of linear? real?)
;;;   distance : non-negative-real?
;;;   updown : one of 'up or 'down
;;; Offset the line up or down so that the distance between the two lines
;;; is distance.
(define offset-line
  (lambda (line distance updown)
    ; (println (list 'offset-line line distance updown))
    (let* ([mult (if (equal? updown 'up) -1 1)]
           [a (linear-slope line)]
           [b (linear-y-intercept line)])
      ; Note: Computation is going from ax + 0 to ax + c
      (if (zero? a)
          (linear a (+ b (* mult distance)))
          (let* ([inverse (/ -1 a)]
                 [x (/ distance (sqrt (+ 1 (sqr inverse))))]
                 [c (* x (+ inverse (- a)))])
            (linear a (+ b (* mult (abs c)))))))))

(define offset-line-old
  (lambda (line distance updown)
    (println (list 'offset-line line distance updown))
    (let* ([mult (if (equal? updown 'up) -1 1)]
           [slope (linear-slope line)])
      (if (zero? slope)
          (linear slope (+ (linear-y-intercept line) (* mult distance)))
          (let* ([inverse (/ -1 slope)]
                 [hoff (sqrt (/ (sqr distance)
                                (+ 1 (sqr inverse))))]
                 [voff (abs (* inverse hoff))])
            (linear slope
                    (+ (linear-apply line hoff)
                       (* mult (abs voff)))))))))

;;; (intersection line1 line2) -> (any-of pt? false?)
;;;   line1 : (any-of linear? real?)
;;;   line2 : (any-of linear? real?)
;;; Find the point of intersection between `line1` and `line2`. Returns
;;; `#f` if they don't intercept. Lines represented as reals are vertical
;;; and the real number is the x intercept.
(define intersection
  (lambda (line1 line2)
    (cond
      [(and (real? line1) (real? line2))
       #f]
      [(real? line1)
       (pt line1 (linear-apply line2 line1))]
      [(real? line2)
       (pt line2 (linear-apply line1 line2))]
      [(equal? (linear-slope line1) (linear-slope line2))
       #f]
      [else
       (let ([x (/ (- (linear-y-intercept line2) (linear-y-intercept line1))
                   (- (linear-slope line1) (linear-slope line2)))])
         (pt x (linear-apply line1 x)))])))

;;; (intersections lines) -> (list-of pt?)
;;;   lines : (list-of (any-of linear? real?))
;;; Find the intersections between successive lines. If two lines are
;;; parallel, skips that intersection.
(define intersections
  (lambda (lines)
    (let kernel ([remaining (append lines (list (car lines)))])
      (if (null? (cdr remaining))
          null
          (let ([point (intersection (car remaining) (cadr remaining))])
            (if point
                (cons point (kernel (cdr remaining)))
                (kernel (cdr remaining))))))))

;;; (expanded-polygon-line pt1 pt2 distance) -> (any-of linear? real?)
;;;   pt1 : point?
;;;   pt2 : point?
;;;   distance : positive-real?
;;; Compute a line in the expanded polygon using the points at the end of the
;;; original line. If the resulting line is vertical, returns the x-intercept
;;; of the point.
(define expanded-polygon-line
  (lambda (pt1 pt2 distance)
    (cond
      [(> (pt-x pt2) (pt-x pt1))
       (offset-line (line-between pt1 pt2) distance 'up)]
      [(< (pt-x pt2) (pt-x pt1))
       (offset-line (line-between pt1 pt2) distance 'down)]
      [(> (pt-y pt2) (pt-y pt1))
       (+ (pt-x pt1) distance)]
      [else
       (- (pt-x pt1) distance)])))

;;; (expand-polygon points distance) -> (list-of pt?)
;;;   points : (list-of pt?)
;;;   distance : non-negative-real?
;;; Expand the points in the polygon by the given distance.
(define expand-polygon
  (lambda (points distance)
    (letrec ([lines (lambda (remaining)
                      (cond
                        [(null? (cdr remaining))
                         null]
                        [(equal? (car remaining) (cadr remaining))
                         (lines (cdr remaining))]
                        [else
                         (cons (expanded-polygon-line (car remaining)
                                                      (cadr remaining)
                                                      distance)
                               (lines (cdr remaining)))]))])
      (let ([expanded-lines (lines (lastfirst points))])
        (intersections (lastfirst expanded-lines))))))

;;; (polygon-lines points) -> (list-of linear?)
;;;   points : list-of point?
;;; Determine the lines that bound a polygon
(define polygon-lines
  (lambda (points)
    (let kernel ([remaining points])
      (cond
        [(null? (cdr remaining))
         (if (equal? (car remaining) (car points))
             null
             (list (line-between (car remaining) (car points))))]
        [(equal? (car remaining) (cadr remaining))
         (kernel (cdr remaining))]
        [else
         (cons (line-between (car remaining) (cadr remaining))
               (kernel (cdr remaining)))]))))

;;; (lastfirst lst) -> list?
;;;   lst : list?
;;; Add the last element in `lst` to the front of the list.
(define lastfirst
  (lambda (lst)
    (cons (last lst) lst)))

; +-------------+----------------------------------------------------
; | Backgrounds |
; +-------------+

;;; (transparent? color) -> boolean?
;;;   color : 2htdp:color?
;;; Determines if an rgb color is transparent.
(define transparent?
  (lambda (color)
    (zero? (rgb-alpha color))))

;;; (column-transparent? bitmap col) -> boolean?
;;;   bitmap : bitmap?
;;;   col : (all-of nonnegative-integer? (less-than (image-width img)))
;;; Determine if the specified column contains only transparent pixels.
(define column-transparent?
  (lambda (bitmap col)
    (let kernel ([row 0])
      (or (>= row (bitmap-height bitmap))
          (and (transparent? (bitmap-get-pixel/kernel bitmap col row))
               (kernel (+ row 1)))))))

;;; (row-transparent? bm row) -> boolean?
;;;   img : bitmap?
;;;   row : (all-of nonnegative-integer? (less-than (image-height img)))
;;; Determine if the specified column contains only transparent pixels.
(define row-transparent?
  (lambda (bm row)
    (let kernel ([col 0])
      (or (>= col (bitmap-width bm))
          (and (transparent? (bitmap-get-pixel bm col row))
               (kernel (+ col 1)))))))

;;; (transparent-rectangle width height) -> 2htdp:image?
;;;   width : nonnegative-integer?
;;;   height : nonnegative-integer?
;; Make a width-by-height transparent rectangle.
(define transparent-rectangle
  (lambda (width height)
    (2htdp:rectangle width height "solid" (2htdp:color 0 0 0 0))))

;;; (add-transparent-background img [suggested-width] [suggested-height]) -> 2htdp:image?
;;;   img : 2htdp:image?
;;; Adds a transparent background to the image in such a way to ensure that
;;; all of the image is visible (within limit of precision).
(define add-transparent-background
  (letrec ([hprocess
            (lambda (img min-width proposed-width height)
              (let kernel ([minw min-width]
                           [propw proposed-width]
                           [maxw #f]
                           [hoff 0])
                ; (displayln (list 'hprocess-kernel minw propw hoff))
                (if (<= propw minw)
                    (cons minw hoff)
                    (let* ([bg (transparent-rectangle propw height)]
                           [tmp (2htdp:overlay/offset img hoff 0 bg)]
                           [bm (2htdp->bitmap tmp)]
                           [drop-left? (column-transparent? bm 0)]
                           [drop-right? (column-transparent? bm (- propw 1))])
                      ; (displayln (list 'drop-left? drop-left? 'drop-right? drop-right?))
                      ; (displayln (2htdp:frame tmp))
                      (cond
                        [; If both ends are transparent, skip 'em.
                         (and drop-left? drop-right?)
                         (kernel minw
                                 (- propw 2)
                                 propw
                                 hoff)]
                        [; If the left has color and the right is clear
                         ; then shift left.
                         drop-right?
                         (if (> hoff 0)
                             (cons propw hoff)
                             (kernel minw propw maxw (- hoff 1)))]
                        [;If the left is clear and the right has color,
                         ; then shift right.
                         drop-left?
                         (if (< hoff 0)
                             (cons propw hoff)
                             (kernel minw propw maxw (+ hoff 1)))]
                        [; If both ends have color, expand.
                         (kernel propw (or maxw (+ propw 8)) maxw 0)])))))]
           [vprocess
            (lambda (img width hoff min-height proposed-height)
              (let kernel ([minh min-height]
                           [proph proposed-height]
                           [maxh #f]
                           [voff 0])
                ; (displayln (list 'vprocess-kernel minh proph voff))
                (if (<= proph minh)
                    (cons minh voff)
                    (let* ([bg (transparent-rectangle width proph)]
                           [tmp (2htdp:overlay/offset img hoff voff bg)]
                           [bm (image->bitmap tmp)]
                           [drop-top? (row-transparent? bm 0)]
                           [drop-bot? (row-transparent? bm (- proph 1))])
                      ; (displayln (2htdp:frame tmp))
                      (cond
                        [; If both ends are transparent, skip 'em
                         (and drop-top? drop-bot?)
                         (kernel minh
                                 (- proph 2)
                                 proph
                                 voff)]
                        [; If the top has color and the bottom is clear,
                         ; shift up
                         drop-bot?
                         (if (> voff 0)
                             (cons proph voff)
                             (kernel minh proph maxh (- voff 1)))]
                        [; If the top is clear and the bottom has color,
                         ; shift down.
                         drop-top?
                         (if (< voff 0)
                             (cons proph voff)
                             (kernel minh proph maxh (+ voff 1)))]
                        [; If both top and bottom have color, expand.
                         (kernel proph (or maxh (+ proph 8)) maxh 0)])))))])
    (lambda (img [suggested-width #f] [suggested-height #f])
      (let* ([min-w (2htdp:image-width img)]
             [min-h (2htdp:image-height img)]
             [w (or suggested-width (+ 8 min-w))]
             [h (or suggested-height (+ 8 min-h))]
             [width-plus-hoff (hprocess img min-w w h)]
             [width (car width-plus-hoff)]
             [hoff (cdr width-plus-hoff)]
             [height-plus-voff (vprocess img width hoff min-h h)]
             [height (car height-plus-voff)]
             [voff (cdr height-plus-voff)])
        (2htdp:overlay/offset img
                              hoff voff
                              (transparent-rectangle width height))))))

; +------+-----------------------------------------------------------
; | Pens |
; +------+

(define pen 2htdp:pen)
(define pen-color 2htdp:pen-color)
(define pen-width 2htdp:pen-width)

;;; (2htdp-style name solid outline mode color-or-pen description) -> image?
;;;   name : symbol
;;;   solid : (color? -> image?)
;;;   outline : (color? real? -> image?)
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Do the 2hdtp-style work
(define 2htdp-style
  (lambda (name solid outlined mode color-or-pen)
    (cond
      [(or (equal? mode "solid") (equal? mode 'solid))
       (when (not (color? color-or-pen))
         (error name "requires a color, received ~a" color-or-pen))
       (solid color-or-pen)]
      [(or (equal? mode "outline") (equal? mode 'outline))
       (cond
         [(color? color-or-pen)
          (outlined color-or-pen 1)]
         [(2htdp:pen? color-or-pen)
          (outlined (2htdp:pen-color color-or-pen)
                    (2htdp:pen-width color-or-pen))]
         [else
          (error name "invalid color-or-pen: ~a" color-or-pen)])]
      [(and (integer? mode) (<= 0 mode 255))
       (when (not (color? color-or-pen))
         (error name "requires a color, received ~a" color-or-pen))
       (let ([tmp (color->rgb color-or-pen)])
         (solid (rgb (rgb-red tmp)
                     (rgb-green tmp)
                     (rgb-blue tmp)
                     (round (* 1/255 mode (rgb-alpha tmp))))))])))

; +----------------+-------------------------------------------------
; | Generic images |
; +----------------+

;;; (image [description] [structure] [bits] [picture]) -> image?
;;;   description : string?
;;;   structure : list?
;;;   bits : (vector-of rgb?)
;;;   picture : 2htdp:image?
;;; Our generic image.
(sstruct %image
         ([desc #:mutable]
          [stru #:mutable]
          [bits #:mutable]
          [pict #:mutable])
         #:transparent
         #:cloneable
         #:methods gen:img-make-pict []
         #:methods gen:img-make-bits []
         #:methods gen:img-make-stru []
         #:methods gen:img-make-desc []
         #:methods gen:img-color []
         #:methods gen:img-fname []
         #:methods gen:img-line-width []
         #:methods gen:img-recolor []
         #:methods gen:img-subimages []
         #:methods gen:custom-write
         [(define write-proc
            (lambda (img port mode)
              (when (markdown-dir)
                (markdown-image img))
              (write (image-picture img) port)))]
         #:done)

(define image
  (lambda ([desc "an image"] [stru #f] [bits #f] [pict #f])
    (%image desc stru bits pict)))

(define image-pict %image-pict)
(define image-bits %image-bits)
(define image-stru %image-stru)
(define image-desc %image-desc)
(define set-image-bits! set-%image-bits!)
(define set-image-pict! set-%image-pict!)
(define set-image-stru! set-%image-stru!)
(define set-image-desc! set-%image-desc!)
(define image? %image?)

;;; (image-bitmap img) -> (vector-of rgb?)
;;;   img : image?
;;; Get the bitmap associated with img.
(define image-bitmap
  (lambda (img)
    (when (not (image-bits img))
      (set-image-bits! img (image-make-bits img)))
    (image-bits img)))

;;; (image-description img) -> string?
;;;   img : image?
;;; Get the description of the image.
(define image-description
  (lambda (img)
    (when (not (image-desc img))
      (set-image-desc! img (image-make-desc img)))
    (image-desc img)))

;;; (describe-image img) -> string?
;;;   img : image?
;;; Get the description of the image.
(define describe-image image-description)

;;; (image-picture img) -> 2htdp:image?
;;;   img : image?
;;; Get the underlying 2htdp image from the image.
(define image-picture
  (lambda (img)
    (when (not (image-pict img))
      (set-image-pict! img (image-make-pict img)))
    (image-pict img)))

;;; (image-structure img) -> list?
;;;   img : image?
;;; Get the structure of the image. In many cases, the structure
;;; of the image should be sufficient to rebuild the image.
(define image-structure
  (lambda (img)
    (when (not (image-stru img))
      (set-image-stru! img (image-make-stru img)))
    (image-stru img)))

;;; (image-height img) -> exact-integer?
;;;   img : image?
;;; Get the height of the image.
(define image-height
  (lambda (img)
    (if (%bitmap? img)
        (%bitmap-height img)
        (2htdp:image-height (image-picture img)))))

;;; (image-width img) -> exact-integer?
;;;   img : image?
;;; Get the width of the image.
(define image-width
  (lambda (img)
    (if (%bitmap? img)
        (%bitmap-width img)
        (2htdp:image-width (image-picture img)))))

;;; (image-clear-fields! img) -> image?
;;;   img : image?
;;; Clear all of the computed fields in `img`.
(define image-clear-fields!
  (lambda (img)
    (set-image-bits! img #f)
    (set-image-desc! img #f)
    (set-image-pict! img #f)
    (set-image-desc! img #f)
    img))

;;; (image-left img) -> real?
;;;   img : image?
;;; Determine the x coordinate of the left edge of img.
;;;
;;; This should always be 0.
(define image-left
  (lambda (img)
    0))

;;; (image-hcenter img) -> real?
;;;   img : image?
;;; Determine the x coordinate of the center of img.
(define image-hcenter
  (lambda (img)
    (* 1/2 (image-width img))))

;;; (image-right img) -> real?
;;;   img : image?
;;; Determine the x coordinate of the right edge of img
(define image-right image-width)

;;; (image-top img) -> real?
;;;   img : image?
;;; Determine the y coordinate of the top edge of img.
;;;
;;; This should always be 0.
(define image-top
  (lambda (img)
    0))

;;; (image-vcenter img) -> real?
;;;   img : image?
;;; Determine the y coordinate of the center of img.
(define image-vcenter
  (lambda (img)
    (* 1/2 (image-height img))))

;;; (image-bottom img) -> real?
;;;   img : image?
;;; Determine the y coordinate of the bottom edge of img
(define image-bottom image-width)

; +--------------+---------------------------------------------------
; | Basic images |
; +--------------+

(sstruct %basic-image %image ([color #:mutable])
         #:transparent
         #:reflection-name 'basic-image
         #:cloneable
         #:methods gen:img-color
         [(define .image-color
            (lambda (img)
              (%basic-image-color img)))]
         #:methods gen:img-recolor
         [(define .image-recolor
            (lambda (img color)
              (let ([result (clone img)])
                (image-clear-fields! result)
                (set-%basic-image-color! result color)
                result)))]
         #:done)

;;; (basic-image? val) -> boolean?
;;;   val : any?
;;; Determine if `val` is one of the basic image types (e.g., shapes).
(define basic-image? %basic-image?)

;;; (basic-image-color img) -> color?
;;;   img : basic-image?
;;; Determine the color of a basic image.
(define basic-image-color %basic-image-color)

;;; (basic-image [color] [description] [structure] [bits] [pict]) -> image?
;;;   color : color?
;;;   description : string?
;;;   structure : list?
;;;   bits : (vector-of rgb?)
;;;   picture : 2htdp:image?
;;; A basic image; intended primarily as a placeholder in the hierarchy.
(define basic-image
  (lambda ([color (rgb 0 0 0 0)]
           [description "a basic image"]
           [structure #f]
           [bits #f]
           [picture #f])
    (%basic-image description structure bits picture (color->rgb color))))

; +--------+---------------------------------------------------------
; | Shapes |
; +--------+

;;; (shape [color] [description] [structure] [bits] [pict]) -> image?
;;;   color : color?
;;;   description : string?
;;;   structure : list?
;;;   bits : (vector-of rgb?)
;;;   picture : 2htdp:image?
;;; A shape; intended primarily as a placeholder in the hierarchy.
(sstruct %shape %basic-image ())

(define shape? %shape?)

(define shape
  (lambda ([color (rgb 0 0 0 0)]
           [description "a shape"]
           [structure #f]
           [bits #f]
           [picture #f])
    (%shape description structure bits picture (color->rgb color))))

; +----------+-------------------------------------------------------
; | Polygons |
; +----------+

(sstruct %polygon %shape (points))

(define polygon? %polygon?)
(define polygon-points %polygon-points)

;;; (polygon-sides poly) -> nonnegative-integer?
;;;   poly : polygon?
;;; Determine the number of sides in the polygon.
(define polygon-sides
  (lambda (poly)
    (length (polygon-points poly))))

;;; (polygon-side poly n) -> real?
;;;   poly : polygon?
;;;   n : (all-of? nonnegative-integer? (cut (< <> (polygon-sides poly))))
;;; Get the length of the nth side in the polygon.
(define polygon-side
  (lambda (poly n)
    (when (not (polygon? poly))
      (error 'polygon-side "Expected a polygon, received ~e" poly))
    (let ([sides (polygon-sides poly)]
          [points (polygon-points poly)])
      (when (>= n sides)
        (error 'polygon-side "Expected a number between 0 (inclusive) and ~a (exclusive), received ~e" sides n))
      (if (= n (- sides 1))
          (distance (car points) (list-ref points n))
          (distance (list-ref points n) (list-ref points (+ n 1)))))))

;;; (polygon-point poly n) -> point?
;;;   poly : polygon?
;;;   n : (all-of nonnegative-integer? (cut (< <> (polygon-sides poly))))
;;; Get the nth point of the polygon.
(define polygon-point
  (lambda (poly n)
    (let kernel ([points (polygon-points poly)]
                 [i n])
      (cond
        [(null? points)
         (error 'polygon-point "~a is too large" n)]
        [(zero? i)
         (car points)]
        [else
         (polygon-point (cdr points) (- i 1))]))))

(sstruct %solid-polygon %polygon ()
         #:cloneable
         #:methods gen:solid []
         #:methods gen:img-fname
         [(define .image-fname
            (lambda (img dir)
              (make-image-fname dir
                                (format "solid-~a-polygon-"
                                        (color->color-name (image-color img))))))]
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (format "a solid ~a polygon built from the points ~e"
                      (color->color-name (image-color img))
                      (polygon-points img))))]
         #:methods gen:img-make-pict
         [(define image-make-pict
            (lambda (img)
              (2htdp:polygon (map pt->posn (polygon-points img))
                             "solid"
                             (color->2htdp (image-color img)))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'solid-polygon (polygon-points img) (image-color img))))]
         #:done)

(define solid-polygon? %solid-polygon?)

;;; (solid-polygon points color [desc]) -> image?
;;;   points : (list-of point?)
;;;   color : color?
;;;   description : string?
;;; A polygon whose vertices are given by `points` and whose color is `color`.
(define solid-polygon
  (lambda (points
           color
           [description #f])
    (let ([color (color->rgb color)])
      (%solid-polygon description
                      #f
                      #f
                      #f
                      color
                      points))))

(sstruct %outlined-polygon %polygon (line-width)
         #:cloneable
         #:mutable
         #:methods gen:outlined []
         #:methods gen:img-fname
         [(define .image-fname
            (lambda (img dir)
              (make-image-fname dir
                                (format "outlined~a-~a-polygon-"
                                        (color->color-name (image-color img))
                                        (line-width img)))))]
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (format "an outlined ~a polygon built from the points ~e"
                      (color->color-name (image-color img))
                      (polygon-points img))))]
         #:methods gen:img-make-pict
         [(define image-make-pict
            (lambda (img)
              (let* ([lw (line-width img)]
                     [points (polygon-points img)]
                     [newpoints (append (reverse (lastfirst (expand-polygon points lw)))
                                        (lastfirst points))]
                     [tmp (2htdp:polygon (map pt->posn newpoints)
                                         "solid"
                                         (color->2htdp (image-color img)))])
                tmp)))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'outlined-polygon
                    (polygon-points img)
                    (image-color img)
                    (line-width img))))]
         #:methods gen:img-line-width
         [(define line-width
            (lambda (img)
              (min 255
                   (max 0
                        (round (%outlined-polygon-line-width img))))))]
         #:done)

(define outlined-polygon? %outlined-polygon?)

;;; (outlined-polygon points color [desc]) -> image?
;;;   points : (list-of point?)
;;;   color : color?
;;;   line-width : nonnegative-integer?
;;;   description : string?
;;; A polygon whose vertices are given by `points`, whose color is `color`,
;;; and whose line width is given by `line-width`.
;;;
;;; Note that this does not work perfectly for polygons with sharp
;;; angels and thicker lines.
(define outlined-polygon
  (lambda (points
           color
           line-width
           [description #f])
    (let ([color (color->rgb color)])
      (%outlined-polygon description
                         #f
                         #f
                         #f
                         color
                         points
                         line-width))))


;;; (polygon points mode color-or-pen [description]) -> image?
;;;   points : (list-of pt?)
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Create the described polygon.
(define polygon
  (lambda (points mode color-or-pen [description #f])
    (2htdp-style 'polygon
                 (lambda (color)
                   (solid-polygon points color description))
                 (lambda (color line-width)
                   (outlined-polygon points color line-width description))
                 mode
                 color-or-pen)))

(define polygon-old
  (lambda (points mode color-or-pen [description #f])
    (cond
      [(or (equal? mode "solid") (equal? mode 'solid))
       (when (not (color? color-or-pen))
         (error 'polygon "solid polygons need a color, received ~a"
                color-or-pen))
       (solid-polygon points color-or-pen description)]
      [(or (equal? mode "outline") (equal? mode 'outline))
       (cond
         [(color? color-or-pen)
          (outlined-polygon points color-or-pen 1)]
         [(2htdp:pen? color-or-pen)
          (outlined-polygon points
                            (2htdp:pen-color color-or-pen)
                            (2htdp:pen-width color-or-pen)
                            description)]
         [else
          (error 'polygon "invalid color-or-pen: ~a" color-or-pen)])]
      [(and (integer? mode) (<= 0 mode 255))
       (when (not (color? color-or-pen))
         (error 'polygon "solid polygons need a color, received ~a"
                color-or-pen))
       (let ([tmp (color->rgb color-or-pen)])
         (solid-polygon points (rgb (rgb-red tmp)
                                    (rgb-green tmp)
                                    (rgb-blue tmp)
                                    (round (* 1/255 mode (rgb-alpha tmp))))
                        description))])))

; +------------+-----------------------------------------------------
; | Rectangles |
; +------------+

(sstruct %solid-rectangle %solid-polygon (width height)
         #:cloneable
         #:methods gen:img-fname
         [(define .image-fname
            (lambda (img dir)
              (format "~a/solid-~a-~ax~a-rectangle.png"
                      (or dir ".")
                      (color->color-name (image-color img))
                      (rectangle-width img)
                      (rectangle-height img))))]
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (format "a solid ~a ~a-by-~a rectangle"
                      (color->color-name (image-color img))
                      (rectangle-width img)
                      (rectangle-height img))))]
         #:methods gen:img-make-pict
         [(define image-make-pict
            (lambda (img)
              (2htdp:polygon (map pt->posn (polygon-points img))
                             "solid"
                             (color->2htdp (image-color img)))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'solid-rectangle
                    (rectangle-width img)
                    (rectangle-height img)
                    (image-color img))))]
         #:done)

(define solid-rectangle? %solid-rectangle?)
(define solid-rectangle-width  %solid-rectangle-width)
(define solid-rectangle-height %solid-rectangle-height)

;;; (solid-rectangle width height color [desc]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   color : color?
;;;   description : string?
;;; A polygon whose vertices are given by `points` and whose color is `color`.
(define solid-rectangle
  (lambda (width
           height
           color
           [description #f])
    (let ([color (color->rgb color)]
          [points (list (pt 0 0) (pt width 0) (pt width height) (pt 0 height))])
      (%solid-rectangle description
                        #f
                        #f
                        #f
                        color
                        points
                        width
                        height))))

(sstruct %outlined-rectangle %outlined-polygon (width height)
         #:cloneable
         #:methods gen:img-fname
         [(define .image-fname
            (lambda (img dir)
              (format "~a/outlined-~a-~a-~ax~a-rectangle.png"
                      (or dir ".")
                      (line-width img)
                      (color->color-name (image-color img))
                      (rectangle-width img)
                      (rectangle-height img))))]
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (format "a ~a-by-~a rectangle outlined with a width-~a ~a line"
                      (rectangle-width img)
                      (rectangle-height img)
                      (line-width img)
                      (color->color-name (image-color img)))))]
         #:methods gen:img-make-pict
         [(define image-make-pict
            (lambda (img)
              (let ([lw (line-width img)])
                (2htdp:overlay (2htdp:rectangle (+ (rectangle-width img) lw)
                                                (+ (rectangle-height img) lw)
                                                "outline"
                                                (2htdp:pen (color->2htdp (image-color img))
                                                           lw
                                                           "solid"
                                                           "round"
                                                           "miter"))
                               (2htdp:rectangle (+ (rectangle-width img) lw lw)
                                                (+ (rectangle-height img) lw lw)
                                                "solid"
                                                (2htdp:color 0 0 0 0))))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'outlined-rectangle
                    (rectangle-width img)
                    (rectangle-height img)
                    (image-color img))))]
         #:done)

(define outlined-rectangle? %outlined-rectangle?)
(define outlined-rectangle-width  %outlined-rectangle-width)
(define outlined-rectangle-height %outlined-rectangle-height)

;;; (outlined-rectangle width height color line-width [description]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   color : color?
;;;   line-width : positive-integer?
;;;   description : string?
;;; A rectangle of the specified width and height, outlined with a
;;; line of the specified color and width.
(define outlined-rectangle
  (lambda (width
           height
           color
           line-width
           [description #f])
    (when (not (nonnegative-real? width))
      (error 'outlined-rectangle "expects a nonnegative width, received ~a" width))
    (when (not (nonnegative-real? height))
      (error 'outlined-rectangle "expects a nonnegative height, received ~a" height))
    (when (not (color? color))
      (error 'outlined-rectangle "expects a color, received ~a" color))
    (when (not (positive-integer? line-width))
      (error 'outlined-rectangle "expects a positive integer, received ~a" line-width))
    (let ([color (color->rgb color)]
          [points (list (pt 0 0) (pt width 0) (pt width height) (pt 0 height))])
      (%outlined-rectangle description
                           #f
                           #f
                           #f
                           color
                           points
                           line-width
                           width
                           height))))

;;; (rectangular-polygon? poly) -> boolean?
;;;   poly : polygon
;;; Determine if a polygon is a rectangle.
(define rectangular-polygon?
  (lambda (poly)
    (and (polygon? poly)
         (let ([points (polygon-points poly)])
           (and (= 4 (length points))
                (let ([pt0 (car points)]
                      [pt1 (cadr points)]
                      [pt2 (caddr points)]
                      [pt3 (cadddr points)])
                  (and (< (abs (- (distance pt0 pt1) (distance pt2 pt3))) 1)
                       (< (abs (- (distance pt1 pt2) (distance pt3 pt0))) 1)
                       (<= 89.9 (abs (angle-between-points pt0 pt1 pt2)) 90.1))))))))

;;; (rectangle? img) -> boolean?
;;;   img : image?
;;; Determine if `img` is a rectangle.
(define rectangle?
  (lambda (img)
    (or (solid-rectangle? img)
        (outlined-rectangle? img)
        (and (polygon? img)
             (rectangular-polygon? img))
        (and (transformed? img)
             (preserved? img)
             (rectangle? (subimage img))))))

;;; (rectangle-height rect) -> real?
;;;   rect : rectangle?
;;; Determine the height of a rectangle.
(define rectangle-height
  (lambda (rect)
    (cond
      [(solid-rectangle? rect)
       (solid-rectangle-height rect)]
      [(outlined-rectangle? rect)
       (outlined-rectangle-height rect)]
      [(and (polygon? rect) (rectangular-polygon? rect))
       (let ([points (polygon-points rect)])
         (distance (cadr points) (caddr points)))]
      [(transformed? rect)
       (rectangle-height (car (subimages rect)))]
      [else
       (error 'rectangle-height "expected a rectangle, received ~e" rect)])))

;;; (rectangle-width rect) -> real?
;;;   rect : rectangle?
;;; Determine the width of a rectangle.
(define rectangle-width
  (lambda (rect)
    (cond
      [(solid-rectangle? rect)
       (solid-rectangle-width rect)]
      [(outlined-rectangle? rect)
       (outlined-rectangle-width rect)]
      [(and (polygon? rect) (rectangular-polygon? rect))
       (let ([points (polygon-points rect)])
         (distance (car points) (cadr points)))]
      [(transformed? rect)
       (rectangle-width (car (subimages rect)))]
      [else
       (error 'rectangle-width "expected a rectangle, received ~e" rect)])))

;;; (rectangle width height mode color-or-pen [description]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Create the described polygon.
(define rectangle
  (lambda (width height mode color-or-pen [description #f])
    (2htdp-style 'rectangle
                 (lambda (color)
                   (solid-rectangle width height color description))
                 (lambda (color line-width)
                   (outlined-rectangle width height color line-width description))
                 mode
                 color-or-pen)))

; +---------+--------------------------------------------------------
; | Squares |
; +---------+

(sstruct %solid-square %solid-rectangle ()
         #:cloneable
         #:methods gen:img-fname
         [(define .image-fname
            (lambda (img dir)
              (format "~a/solid-~a-square-~a.png"
                      (or dir ".")
                      (color->color-name (image-color img))
                      (square-side img))))]
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (format "a solid ~a square with side length ~a"
                      (color->color-name (image-color img))
                      (image-width img))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'solid-square (rectangle-width img) (image-color img))))]
         #:done)

(define solid-square? %solid-square?)

;;; (solid-square side color [description]) -> image?
;;;   side : nonnegative-integer?
;;;   color : color?
;;;   description : string?
;;; Create a solid square with the given side length and color.
(define solid-square
  (lambda (side color [description #f])
    (let ([color (color->rgb color)]
          [points (list (pt 0 0) (pt side 0) (pt side side) (pt 0 side))])
      (%solid-square description
                     #f
                     #f
                     #f
                     color
                     points
                     side
                     side))))

(sstruct %outlined-square %outlined-rectangle ()
         #:cloneable
         #:methods gen:img-fname
         [(define .image-fname
            (lambda (img dir)
              (format "~a/outlined-~a-~a-square-~a.png"
                      (or dir ".")
                      (line-width img)
                      (color->color-name (image-color img))
                      (square-side img))))]
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (format "an outlined ~a square with side length ~a"
                      (color->color-name (image-color img))
                      (image-width img))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'outlined-square (rectangle-width img) (image-color img))))]
         #:done)

(define outlined-square? %outlined-square?)

;;; (outlined-square side color line-width [description]) -> image?
;;;   side : nonnegative-integer?
;;;   color : color?
;;;   line-width : positive-real?
;;;   description : string?
;;; Create an outlined square with the given side length and color.
(define outlined-square
  (lambda (side color line-width [description #f])
    (let ([color (color->rgb color)]
          [points (list (pt 0 0) (pt side 0) (pt side side) (pt 0 side))])
      (%outlined-square description
                        #f
                        #f
                        #f
                        color
                        points
                        line-width
                        side
                        side))))

;;; (square? img) -> boolean?
;;;   img : image?
;;; Determine if the image is a square.
(define square?
  (lambda (img)
    (or (solid-square? img)
        (outlined-square? img)
        (and (rectangle? img)
             (<= (abs (- (rectangle-width img) (rectangle-height img))) 0.01))
        (and (transformed? img)
             (preserved? img)
             (square? (subimage img))))))

;;; (square-side sq) -> real?
;;;   sq : square?
;;; Determine the side length of a square.
(define square-side
  (lambda (sq)
    (rectangle-width sq)))

;;; (square edge mode color-or-pen [description]) -> image?
;;;   edge : nonnegative-real?
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Create the described square.
(define square
  (lambda (edge mode color-or-pen [description #f])
    (2htdp-style 'square
                 (lambda (color)
                   (solid-square edge color description))
                 (lambda (color line-width)
                   (outlined-square edge color line-width description))
                 mode
                 color-or-pen)))

; +----------+-------------------------------------------------------
; | Diamonds |
; +----------+

(sstruct %solid-diamond %solid-polygon (width height)
         #:cloneable
         #:methods gen:img-fname
         [(define .image-fname
            (lambda (img dir)
              (format "~a/solid-~a-~ax~a-diamond.png"
                      (or dir ".")
                      (color->color-name (image-color img))
                      (diamond-width img)
                      (diamond-height img))))]
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (format "a solid ~a ~a-by-~a diamond"
                      (color->color-name (image-color img))
                      (diamond-width img)
                      (diamond-height img))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'solid-diamond
                    (diamond-width img)
                    (diamond-height img)
                    (image-color img))))]
         #:done)

(define solid-diamond? %solid-diamond?)
(define solid-diamond-width  %solid-diamond-width)
(define solid-diamond-height %solid-diamond-height)

;;; (diamond-points width height) -> (list-of point?)
;;;   width :positive-real?
;;;   height : positive-real?
;;; Determine the points for a width-by-height diamond.
(define diamond-points
  (lambda (width height)
    (let* ([w (/ width 2)]
           [h (/ height 2)])
      (list (pt w 0) (pt width h) (pt w height) (pt 0 h)))))

;;; (solid-diamond width height color [desc]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   color : color?
;;;   description : string?
;;; A polygon whose vertices are given by `points` and whose color is `color`.
(define solid-diamond
  (lambda (width
           height
           color
           [description #f])
    (%solid-diamond description
                    #f
                    #f
                    #f
                    (color->rgb color)
                    (diamond-points width height)
                    width
                    height)))

(sstruct %outlined-diamond %outlined-polygon (width height)
         #:cloneable
         #:methods gen:img-fname
         [(define .image-fname
            (lambda (img dir)
              (format "~a/outlined-~a-~a-~ax~a-diamond.png"
                      (or dir ".")
                      (color->color-name (image-color img))
                      (line-width img)
                      (diamond-width img)
                      (diamond-height img))))]
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (format "an outlined ~a ~a-by-~a diamond"
                      (color->color-name (image-color img))
                      (diamond-width img)
                      (diamond-height img))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'outlined-diamond
                    (diamond-width img)
                    (diamond-height img)
                    (image-color img)
                    (line-width img))))]
         #:done)

(define outlined-diamond? %outlined-diamond?)
(define outlined-diamond-width %outlined-diamond-width)
(define outlined-diamond-height %outlined-diamond-height)

;;; (outlined-diamond width height color line-width [desc]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   color : color?
;;;   line-width : positive-integer?
;;;   description : string?
;;; An outlined diamond whose inner size is width-by-height with
;;; an outlined of `line-width`.
(define outlined-diamond
  (lambda (width
           height
           color
           line-width
           [description #f])
    (%outlined-diamond description
                       #f
                       #f
                       #f
                       (color->rgb color)
                       (diamond-points width height)
                       line-width
                       width
                       height)))

;;; (diamond-polygon? poly) -> boolean?
;;;   poly : polygon?
;;; Determine if poly is a diamond.
(define diamond-polygon?
  (lambda (poly)
    (and (polygon? poly)
         (= 4 (polygon-sides poly))
         (= (polygon-side poly 0)
            (polygon-side poly 1)
            (polygon-side poly 2)
            (polygon-side poly 3)))))

;;; (diamond? img) -> boolean?
;;;   img : image?
;;; Determine if `img` is a diamond.
(define diamond?
  (lambda (img)
    (or (solid-diamond? img)
        (outlined-diamond? img)
        (diamond-polygon? img)
        (and (transformed? img)
             (preserved? img)
             (diamond? (subimage img))))))

;;; (diamond-height d) -> real?
;;;   d : diamond?
;;; Determine the height of a diamond
(define diamond-height
  (lambda (d)
    (cond
      [(solid-diamond? d)
       (solid-diamond-height d)]
      [(outlined-diamond? d)
       (outlined-diamond-height d)]
      [(diamond-polygon? d)
       (distance (polygon-point d 1) (polygon-point d 3))]
      [(and (transformed? d) (preserved? d))
       (diamond-height (subimage d))]
      [else
       (error 'diamond-height "not a diamond ~a" d)])))

;;; (diamond-width d) -> real?
;;;   d : diamond?
;;; Determine the width of a diamond
(define diamond-width
  (lambda (d)
    (cond
      [(solid-diamond? d)
       (solid-diamond-width d)]
      [(outlined-diamond? d)
       (outlined-diamond-width d)]
      [(diamond-polygon? d)
       (distance (polygon-point d 0) (polygon-point d 2))]
      [(and (transformed? d) (preserved? d))
       (diamond-width (subimage d))]
      [else
       (error 'diamond-width "not a diamond ~a" d)])))

;;; (diamond width height mode color-or-pen [description]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Create the described diamond.
(define diamond
  (lambda (width height mode color-or-pen [description #f])
    (2htdp-style 'diamond
                 (lambda (color)
                   (solid-diamond width height color description))
                 (lambda (color line-width)
                   (outlined-diamond width height color line-width description))
                 mode
                 color-or-pen)))

; +---------------------+--------------------------------------------
; | Isosceles triangles |
; +---------------------+

(sstruct %solid-isosceles-triangle %solid-polygon (width height)
         #:cloneable
         #:methods gen:img-fname
         [(define .image-fname
            (lambda (img dir)
              (format "~a/solid-~a-~ax~a-isosceles-triangle.png"
                      (or dir ".")
                      (color->color-name (image-color img))
                      (isosceles-triangle-width img)
                      (isosceles-triangle-height img))))]
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (format "a solid ~a ~a-by-~a isosceles triangle"
                      (color->color-name (image-color img))
                      (isosceles-triangle-width img)
                      (isosceles-triangle-height img))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'solid-isosceles-triangle
                    (isosceles-triangle-width img)
                    (isosceles-triangle-height img)
                    (image-color img))))]
         #:done)

(define solid-isosceles-triangle? %solid-isosceles-triangle?)
(define solid-isosceles-triangle-width  %solid-isosceles-triangle-width)
(define solid-isosceles-triangle-height %solid-isosceles-triangle-height)

;;; (isosceles-triangle-points width height) -> (list-of pt?)
;;;   width : positive-real?
;;;   height : positive-real?
;;; Compute the points in an isosceles triangle of the given
;;; width and height.
(define isosceles-triangle-points
  (lambda (width height)
    (list (pt 0 height) (pt (/ width 2) 0) (pt width height))))

;;; (solid-isosceles-triangle width height color [desc]) -> image?
;;;   width : positive-real?
;;;   height : positive-real?
;;;   color : color?
;;;   description : string?
;;; A solid isosceles triangle of the given width, height, and color.
(define solid-isosceles-triangle
  (lambda (width
           height
           color
           [description #f])
    (%solid-isosceles-triangle description
                               #f
                               #f
                               #f
                               (color->rgb color)
                               (isosceles-triangle-points width height)
                               width
                               height)))

(sstruct %outlined-isosceles-triangle %outlined-polygon (width height)
         #:cloneable
         #:methods gen:img-fname
         [(define .image-fname
            (lambda (img dir)
              (format "~a/outlined-~a-~a-~ax~a-isosceles-triangle.png"
                      (or dir ".")
                      (color->color-name (image-color img))
                      (line-width img)
                      (isosceles-triangle-width img)
                      (isosceles-triangle-height img))))]
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (format "an outlined ~a ~a-by-~a isosceles-triangle"
                      (color->color-name (image-color img))
                      (isosceles-triangle-width img)
                      (isosceles-triangle-height img))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'outlined-isosceles-triangle
                    (isosceles-triangle-width img)
                    (isosceles-triangle-height img)
                    (image-color img)
                    (line-width img))))]
         #:done)

(define outlined-isosceles-triangle? %outlined-isosceles-triangle?)
(define outlined-isosceles-triangle-width %outlined-isosceles-triangle-width)
(define outlined-isosceles-triangle-height %outlined-isosceles-triangle-height)

;;; (outlined-isosceles-triangle width height color line-width [desc]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   color : color?
;;;   line-width : positive-integer?
;;;   description : string?
;;; An outlined isosceles-triangle whose inner size is width-by-height with
;;; an outlined of `line-width`.
(define outlined-isosceles-triangle
  (lambda (width
           height
           color
           line-width
           [description #f])
    (%outlined-isosceles-triangle description
                                  #f
                                  #f
                                  #f
                                  (color->rgb color)
                                  (isosceles-triangle-points width height)
                                  line-width
                                  width
                                  height)))

;;; (isosceles-triangle-polygon? poly) -> boolean?
;;;   poly : polygon?
;;; Determine if poly is a isosceles-triangle.
;;;
;;; Not currently implemented.
(define isosceles-triangle-polygon?
  (lambda (poly)
    #f))

;;; (isosceles-triangle? img) -> boolean?
;;;   img : image?
;;; Determine if `img` is an isosceles-triangle.
(define isosceles-triangle?
  (lambda (img)
    (or (solid-isosceles-triangle? img)
        (outlined-isosceles-triangle? img)
        (and (transformed? img)
             (preserved? img)
             (isosceles-triangle? (subimage img))))))

;;; (isosceles-triangle-height d) -> real?
;;;   tri : isosceles-triangle?
;;; Determine the height of an isosceles-triangle
(define isosceles-triangle-height
  (lambda (tri)
    (cond
      [(solid-isosceles-triangle? tri)
       (solid-isosceles-triangle-height tri)]
      [(outlined-isosceles-triangle? tri)
       (outlined-isosceles-triangle-height tri)]
      [(and (transformed? tri) (preserved? tri))
       (isosceles-triangle-height (subimage tri))]
      [else
       (error 'isosceles-triangle-height "not an isosceles-triangle ~a" tri)])))

;;; (isosceles-triangle-width tri) -> real?
;;;   tri : isosceles-triangle?
;;; Determine the width of an isosceles-triangle
(define isosceles-triangle-width
  (lambda (tri)
    (cond
      [(solid-isosceles-triangle? tri)
       (solid-isosceles-triangle-width tri)]
      [(outlined-isosceles-triangle? tri)
       (outlined-isosceles-triangle-width tri)]
      [(and (transformed? tri) (preserved? tri))
       (isosceles-triangle-width (subimage tri))]
      [else
       (error 'isosceles-triangle-width "not an isosceles-triangle ~a" tri)])))

;;; (isosceles-triangle width height mode color-or-pen [description]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Create the described polygon.
(define isosceles-triangle
  (lambda (width height mode color-or-pen [description #f])
    (2htdp-style 'isosceles-triangle
                 (lambda (color)
                   (solid-isosceles-triangle width height color description))
                 (lambda (color line-width)
                   (outlined-isosceles-triangle width height color line-width description))
                 mode
                 color-or-pen)))

; +-----------------------+------------------------------------------
; | Equilateral triangles |
; +-----------------------+

(sstruct %solid-equilateral-triangle %solid-isosceles-triangle ()
         #:cloneable
         #:methods gen:img-fname
         [(define .image-fname
            (lambda (img dir)
              (format "~a/solid-~a-~a-equilateral-triangle.png"
                      (or dir ".")
                      (color->color-name (image-color img))
                      (equilateral-triangle-edge img))))]
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (format "a solid ~a equilateral triangle with edge length ~a"
                      (color->color-name (image-color img))
                      (equilateral-triangle-edge img))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'solid-equilateral-triangle
                    (equilateral-triangle-edge img)
                    (image-color img))))]
         #:done)

;;; (solid-equilateral-triangle? val) -> boolean?
;;;   val : any?
;;; Determines if `val` is a solid equilateral triangle.
(define solid-equilateral-triangle? %solid-equilateral-triangle?)

;;; (solid-equilateral-triangle edge color [description]) -> image?
;;;   edge: positive-real?
;;;   color : color?
;;;   description : string?
;;; A solid equilateral triangle of the given edge length and color.
(define solid-equilateral-triangle
  (lambda (edge color [description #f])
    (let ([width edge]
          [height (* sqrt-3 1/2 edge)])
      (%solid-equilateral-triangle description
                                   #f
                                   #f
                                   #f
                                   (color->rgb color)
                                   (isosceles-triangle-points width height)
                                   width
                                   height))))

(sstruct %outlined-equilateral-triangle %outlined-isosceles-triangle ()
         #:cloneable
         #:methods gen:img-fname
         [(define .image-fname
            (lambda (img dir)
              (format "~a/outlined-~a-~a-~a-equilateral-triangle.png"
                      (or dir ".")
                      (color->color-name (image-color img))
                      (line-width img)
                      (equilateral-triangle-edge img))))]
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (format "an outlined ~a equilateral-triangle with edge-length ~a"
                      (color->color-name (image-color img))
                      (equilateral-triangle-edge img))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'outlined-equilateral-triangle
                    (equilateral-triangle-edge img)
                    (image-color img)
                    (line-width img))))]
         #:done)

;;; (outlined-equilateral-triangle? val) -> boolean?
;;;   val : any?
;;; Determine if `val` is an equilateral triangle.
(define outlined-equilateral-triangle? %outlined-equilateral-triangle?)

;;; (outlined-equilateral-triangle width height color line-width [desc]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   color : color?
;;;   line-width : positive-integer?
;;;   description : string?
;;; An outlined equilateral-triangle whose inner size is width-by-height with
;;; an outline of `line-width`. The inner triangle is transparent.
(define outlined-equilateral-triangle
  (lambda (edge color line-width [description #f])
    (let ([width edge]
          [height (* sqrt-3 1/2 edge)])
      (%outlined-equilateral-triangle description
                                      #f
                                      #f
                                      #f
                                      (color->rgb color)
                                      (isosceles-triangle-points width height)
                                      line-width
                                      width
                                      height))))

;;; (equilateral-triangle-polygon? poly) -> boolean?
;;;   poly : polygon?
;;; Determine if poly is a equilateral-triangle.
;;;
;;; Not currently implemented.
(define equilateral-triangle-polygon?
  (lambda (poly)
    #f))

;;; (equilateral-triangle? img) -> boolean?
;;;   img : image?
;;; Determine if `img` is an equilateral-triangle.
(define equilateral-triangle?
  (lambda (img)
    (or (solid-equilateral-triangle? img)
        (outlined-equilateral-triangle? img)
        (and (transformed? img)
             (preserved? img)
             (equilateral-triangle? (subimage img))))))

;;; (equilateral-triangle-edge tri) -> real?
;;;   tri : equilateral-triangle?
;;; Determine the edge length of an equilateral-triangle
(define equilateral-triangle-edge
  (lambda (tri)
    (when (not (equilateral-triangle? tri))
      (error 'equilateral-triangle-edge
             "expects an equilateral triangle, received ~a"
             tri))
    (isosceles-triangle-width tri)))

;;; (equilateral-triangle edge mode color-or-pen [description]) -> image?
;;;   edge : nonnegative-real?
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Create the described triangle.
(define equilateral-triangle
  (lambda (edge mode color-or-pen [description #f])
    (2htdp-style 'equilateral-triangle
                 (lambda (color)
                   (solid-equilateral-triangle edge color description))
                 (lambda (color line-width)
                   (outlined-equilateral-triangle edge color line-width description))
                 mode
                 color-or-pen)))

; +----------+-------------------------------------------------------
; | Ellipses |
; +----------+

(sstruct %ellipse %shape (width height)
         #:cloneable)

(sstruct %solid-ellipse %ellipse ()
         #:cloneable
         #:methods gen:solid []
         #:methods gen:img-fname
         [(define .image-fname
            (lambda (img dir)
              (format "~a/solid-~a-ellipse-~ax~a.png"
                      (or dir ".")
                      (color->color-name (image-color img))
                      (image-width img)
                      (image-height img))))]
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (format "a solid ~a ~a-by-~a ellipse"
                      (color->color-name (image-color img))
                      (ellipse-width img)
                      (ellipse-height img))))]
         #:methods gen:img-make-pict
         [(define image-make-pict
            (lambda (img)
              (2htdp:ellipse (ellipse-width img)
                             (ellipse-height img)
                             "solid"
                             (color->2htdp (image-color img)))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'solid-ellipse
                    (ellipse-width img)
                    (ellipse-height img)
                    (image-color img))))]
         #:done)

(define solid-ellipse? %solid-ellipse?)
(define solid-ellipse-width %ellipse-width)
(define solid-ellipse-height %ellipse-height)

;;; (solid-ellipse width height color [description]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   color : color?
;;;   description : string?
;;; A `width`-by-`height` ellipse whose color is `color`.
(define solid-ellipse
  (lambda (width
           height
           color
           [description #f])
    (let ([color (color->rgb color)])
      (%solid-ellipse description
                      #f
                      #f
                      #f
                      color
                      width
                      height))))

(sstruct %outlined-ellipse %ellipse (line-width)
         #:cloneable
         #:methods gen:outlined []
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (format "an outlined ~a ~a-by-~a ellipse"
                      (color->color-name (image-color img))
                      (ellipse-width img)
                      (ellipse-height img))))]
         #:methods gen:img-make-pict
         [(define image-make-pict
            (lambda (img)
              (let* ([lw (line-width img)]
                     [tmp (2htdp:ellipse (+ (ellipse-width img) lw)
                                         (+ (ellipse-height img) lw)
                                         "outline"
                                         (2htdp:pen (color->2htdp (image-color img))
                                                    lw
                                                    "solid"
                                                    "round"
                                                    "miter"))])
                (2htdp:overlay tmp
                               (transparent-rectangle (+ (ellipse-width img) lw lw)
                                                      (+ (ellipse-height img) lw lw))))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'outlined-ellipse
                    (ellipse-width img)
                    (ellipse-height img)
                    (image-color img))))]
         #:methods gen:img-line-width
         [(define line-width
            (lambda (img)
              (max 0
                   (min 255
                        (round (%outlined-ellipse-line-width img))))))]
         #:done)

(define outlined-ellipse? %outlined-ellipse?)
(define outlined-ellipse-width %ellipse-width)
(define outlined-ellipse-height %ellipse-height)

;;; (outlined-ellipse width height color line-width [description]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   color : color?
;;;   line-width : positive-integer?
;;;   description : string?
;;; A `width`-by-`height` ellipse whose color is `color`.
(define outlined-ellipse
  (lambda (width
           height
           color
           line-width
           [description #f])
    (let ([color (color->rgb color)])
      (%outlined-ellipse description
                         #f
                         #f
                         #f
                         color
                         width
                         height
                         line-width))))

;;; (ellipse? img) -> boolean?
;;;   img : image?
;;; Determine if an image is an ellipse.
(define ellipse?
  (lambda (img)
    (or (solid-ellipse? img)
        (outlined-ellipse? img)
        (and (transformed? img)
             (preserved? img)
             (ellipse? (subimage img))))))

;;; (ellipse-width ell) -> nonnegative-real?
;;;   ell : ellipse?
;;; Determine the width of an ellipse. For outlined ellipses, this is
;;; the width of the inner ellipse.
(define ellipse-width
  (lambda (img)
    (cond
      [(%ellipse? img)
       (%ellipse-width img)]
      [(transformed? img)
       (ellipse-width (subimage img))]
      [else
       (error 'ellipse-width "expected an ellipse, received ~e" img)])))

;;; (ellipse-height ell) -> nonnegative-real?
;;;   ell : ellipse?
;;; Determine the height of an ellipse. For outlined ellipses, this is
;;; the height of the inner ellipse.
(define ellipse-height
  (lambda (img)
    (cond
      [(%ellipse? img)
       (%ellipse-height img)]
      [(transformed? img)
       (ellipse-height (subimage img))]
      [else
       (error 'ellipse-height "expected an ellipse, received ~e" img)])))

;;; (ellipse width height mode color-or-pen [description]) -> image?
;;;   width : nonnegative-real?
;;;   height : nonnegative-real?
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Create the described ellipse.
(define ellipse
  (lambda (width height mode color-or-pen [description #f])
    (2htdp-style 'ellipse
                 (lambda (color)
                   (solid-ellipse width height color description))
                 (lambda (color line-width)
                   (outlined-ellipse width height color line-width description))
                 mode
                 color-or-pen)))

; +---------+--------------------------------------------------------
; | Circles |
; +---------+

(sstruct %solid-circle %solid-ellipse ()
         #:cloneable
         #:methods gen:img-fname
         [(define .image-fname
            (lambda (img dir)
              (format "~a/solid-~a-circle-~a.png"
                      (or dir ".")
                      (color->color-name (image-color img))
                      (circle-diameter img))))]
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (format "a solid ~a circle with diameter ~a"
                      (color->color-name (image-color img))
                      (ellipse-width img))))]
         #:methods gen:img-make-pict
         [(define image-make-pict
            (lambda (img)
              (2htdp:ellipse (ellipse-width img)
                             (ellipse-height img)
                             "solid"
                             (color->2htdp (image-color img)))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'solid-circle (ellipse-width img) (image-color img))))]
         #:done)

(define solid-circle? %solid-circle?)

;;; (solid-circle diameter color [description]) -> image?
;;;   diameter : nonnegative-integer?
;;;   color : color?
;;;   description : string?
;;; Create a solid circle with the given diameter and color.
(define solid-circle
  (lambda (diam color [description #f])
    (%solid-circle description
                   #f
                   #f
                   #f
                   color
                   diam
                   diam)))

(sstruct %outlined-circle %outlined-ellipse ()
         #:cloneable
         #:methods gen:img-fname
         [(define .image-fname
            (lambda (img dir)
              (format "~a/outlined-~a-~a-circle-~a.png"
                      (or dir ".")
                      (line-width img)
                      (color->color-name (image-color img))
                      (circle-diameter img))))]
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (format "an outlined ~a circle with diameter ~a"
                      (color->color-name (image-color img))
                      (ellipse-width img))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'outlined-circle
                    (ellipse-width img)
                    (image-color img)
                    (line-width img))))]
         #:done)

(define outlined-circle? %outlined-circle?)

;;; (outlined-circle diameter color [description]) -> image?
;;;   diameter : nonnegative-integer?
;;;   color : color?
;;;   line-width : positive-integer?
;;;   description : string?
;;; Create an outlined circle with the given diameter and color.
(define outlined-circle
  (lambda (diam color line-width [description #f])
    (%outlined-circle description
                      #f
                      #f
                      #f
                      color
                      diam
                      diam
                      line-width)))

;;; (circle? img) -> boolean?
;;;   img : image?
;;; Determine if the image is a circle.
(define circle?
  (lambda (img)
    (or (solid-circle? img)
        (outlined-circle? img)
        (and (ellipse? img)
             (<= (abs (- (ellipse-width img) (ellipse-height img))) 0.01))
        (and (transformed? img)
             (preserved? img)
             (circle? (subimage img))))))

;;; (circle-diameter circ) -> real?
;;;   circ : circle?
;;; Determine the diameter of a circle.
(define circle-diameter
  (lambda (circ)
    (ellipse-width circ)))

;;; (circle diameter mode color-or-pen [description]) -> image?
;;;   diameter : nonnegative-real?
;;;   mode : (one-of "solid" "outline" integer?)
;;;   color-or-pen : (any-of color? pen?)
;;;   description : string?
;;; Create the described circle.
(define circle
  (lambda (diameter mode color-or-pen [description #f])
    (2htdp-style 'circle
                 (lambda (color)
                   (solid-circle diameter color description))
                 (lambda (color line-width)
                   (outlined-circle diameter color line-width description))
                 mode
                 color-or-pen)))

; +---------+--------------------------------------------------------
; | Bitmaps |
; +---------+

(sstruct %bitmap %image (width height)
         #:transparent
         #:cloneable
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              "a bitmap"))]
         #:methods gen:img-make-pict
         [(define image-make-pict
            (lambda (img)
              (2htdp:color-list->bitmap (map rgb->2htdp
                                             (vector->list (image-bitmap img)))
                                        (image-width img)
                                        (image-height img))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (list 'bitmap (image-width img) (image-height img))))]
         #:done)

(define bitmap? %bitmap?)
(define bitmap-width %bitmap-width)
(define bitmap-height %bitmap-height)

;;; (image-compute pos2color width height description) -> image?
;;;   pos2color : procedure?
;;;   width : positive-integer?
;;;   height : positive-integer?
;;;   description : string?
;;; Creates a new `width`-by-`height` bitmap by applying `pos2color` to
;;; each (col,row) coordinate to determine the color at that position.
;;;
;;; (pos2color col row) -> rgb?
;;;   col : non-negative-integer?
;;;   row : non-negative-integer?
;;; Compute a color from (col,row)
(define image-compute
  (lambda (pos2color width height description)
    (param-check! image-compute 1 procedure? pos2color)
    (param-check! image-compute 2 positive-integer? width)
    (param-check! image-compute 3 positive-integer? height)
    (param-check! image-compute 4 string? description)
    (let ([pixels (make-vector (* width height))])
      (let kernel ([pos 0]
                   [col 0]
                   [row 0])
        (cond
          [(>= col width)
           (kernel pos 0 (+ row 1))]
          [(< row height)
           (vector-set! pixels pos (pos2color col row))
           (kernel (+ pos 1) (+ col 1) row )]))
      (%bitmap description
               #f
               pixels
               #f
               width
               height))))

;;; (bitmap width height description) -> image?
;;;   width : positive-integer?
;;;   height : positive-integer?
;;;   description : string?
;;; Creates a new `width`-by-`height` transparent bitmap.
(define bitmap
  (lambda (width height description)
    (param-check! image-compute 1 positive-integer? width)
    (param-check! image-compute 2 positive-integer? height)
    (param-check! image-compute 3 string? description)
    (%bitmap description
             #f
             (make-vector (* width height) (rgb 0 0 0 0))
             #f
             width
             height)))

;;; (bitmap-get-pixel bitmap col row) -> color?
;;;   bitmap : bitmap?
;;;   col : (all-of nonnegative-integer? (less-than (bitmap-width bitmap)))
;;;   row : (all-of nonnegative-integer? (less-than (bitmap-height bitmap)))
;;; Get the color of the pixel at position (`col`,`row`) in `bitmap`.
(define bitmap-get-pixel/kernel
  (lambda (bitmap col row)
    (vector-ref (%image-bits bitmap)
                (+ col (* row (%bitmap-width bitmap))))))

(define bitmap-get-pixel
  (lambda (bitmap col row)
    (param-check! bitmap-get-pixel 1 bitmap? bitmap)
    (param-check! bitmap-get-pixel 2 nonnegative-integer? col)
    (param-check! bitmap-get-pixel 2 (less-than (image-width bitmap)) col)
    (param-check! bitmap-get-pixel 3 nonnegative-integer? row)
    (param-check! bitmap-get-pixel 3 (less-than (image-height bitmap)) row)
    (bitmap-get-pixel/kernel bitmap col row)))

;;; (bitmap-set-pixel! bitmap col row color) -> void?
;;;   bitmap : bitmap?
;;;   col : (all-of non-negative-integer? (cut (< <> (image-width img))))
;;;   row : (all-of non-negative-integer? (cut (< <> (image-height img))))
;;;   color : rgb?
;;; Set the specified pixel of the bitmap.
(define bitmap-set-pixel!/kernel
  (lambda (bitmap col row color)
    (vector-set! (%image-bits bitmap)
                 (+ col (* row (%bitmap-width bitmap)))
                 color)
    (set-image-pict! bitmap #f)))

(define bitmap-set-pixel!
  (lambda (bitmap col row color)
    (param-check! bitmap-set-pixel! 1 bitmap? bitmap)
    (param-check! bitmap-set-pixel! 2 nonnegative-integer? col)
    (param-check! bitmap-set-pixel! 2 (less-than (image-width bitmap)) col)
    (param-check! bitmap-set-pixel! 3 nonnegative-integer? row)
    (param-check! bitmap-set-pixel! 3 (less-than (image-height bitmap)) row)
    (param-check! bitmap-set-pixel! 4 rgb? color)
    (bitmap-set-pixel!/kernel bitmap col row color)))

;;; (2htdp->bitmap image) -> bitmap?
;;;   img : 2htdp:image?
;;; Convert a 2htdp image to a bitmap
(define 2htdp->bitmap
  (lambda (img)
    (let* ([tmp (%bitmap "an image"
                         #f
                         #f
                         img
                         (2htdp:image-width img)
                         (2htdp:image-height img))])
      ; The next line forces the creation of the bitmap
      (image-bitmap tmp)
      tmp)))

;;; (image->bitmap image) -> bitmap?
;;;   img : image?
;;; Convert an image into a bitmap.  (E.g., so that we can get and
;;; set pixels.)
(define image->bitmap
  (lambda (img)
    (%bitmap (image-description img)
             #f
             (image-bitmap img)
             #f
             (image-width img)
             (image-height img))))

; +-----------------+------------------------------------------------
; | Transformations |
; +-----------------+

(sstruct %transformed %image ([img #:mutable])
         #:transparent
         #:cloneable
         #:methods gen:img-subimages
         [(define subimages
            (lambda (img)
              (list (%transformed-img img))))]
         #:methods gen:img-color
         [(define .image-color
            (lambda (img)
              (image-color (%transformed-img img))))]
         #:methods gen:img-recolor
         [(define .image-recolor
            (lambda (img color)
              (let ([result (clone img)])
                (image-clear-fields! result)
                (set-%transformed-img! result
                                       (image-recolor (%transformed-img img)
                                                      color))
                result)))]
         #:done)


;;; (transformed-image? val) -> boolean?
;;;   val : any?
;;; Determines if `val` is an image that has been created by transforming
;;; another image (e.g., by scaling or rotating).
(define transformed-image? %transformed?)
(define transformed? %transformed?)

;;; (transformed-subimage? img) -> image?
;;;   img : transformed-image?
;;; Determine what image was transformed to build `img`.
(define subimage %transformed-img)

;;; (transformed-set-image! img subimg) -> void?
;;;   img : transformed-image?
;;;   subimg : image?
;;; Set the image that is transformed.
;;;
;;; This operation is dangerous and should generally only be used with a
;;; clone of an image.
(define transformed-set-image!
  (lambda (img subimg)
    (image-clear-fields! img)
    (set-%transformed-img! img subimg)
    img))

; +----------------------------------+-------------------------------
; | Image-preserving transformations |
; +----------------------------------+

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
              (2htdp:rotate (rotated-angle img)
                            (image-picture (subimage img)))))]
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

; +--------------------------------+---------------------------------
; | Image-altering transformations |
; +--------------------------------+

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
    (%cropped description
              #f
              #f
              #f
              img
              left
              top
              width
              height)))

; +--------------+---------------------------------------------------
; | Combinations |
; +--------------+

(sstruct %compound %image ([images #:mutable])
         #:transparent
         #:cloneable
         #:methods gen:img-subimages
         [(define subimages
            (lambda (img)
              (%compound-images img)))]
         #:methods gen:img-recolor
         [(define .image-recolor
            (lambda (img color)
              (let ([result (clone img)])
                (image-clear-fields! result)
                (set-%compound-images! result
                                       (map (lambda (i)
                                              (image-recolor i color))
                                            (%compound-images img)))
                result)))]
         #:done)

;;; (compound-image? val) -> boolean?
;;;   val : any?
;;; Determines if `val` is a compound image, one build by combining
;;; other images (e.g., with `overlay`, `beside`, or `above`).
(define compound-image? %compound?)
(define compound? %compound?)

;;; (compound-set-images! img images) -> void?
;;;   img : compound-image?
;;;   images : (list-of image?)
;;; Set the images within a compound image.
;;;
;;; This operation is dangerous and should generally only be used with a
;;; clone of an image.
(define compound-set-images!
  (lambda (img images)
    (image-clear-fields! img)
    (set-%compound-images! img images)
    img))

;;; (halignment? val) -> boolean?
;;;   val : any?
;;; Determine if `val` is one of the legal horizontal alignments
;;; ("left", "center', or "right").
(define halignment?
  (one-of "left" "center" "right"))

;;; (valignment? val) -> boolean?
;;;   val : any?
;;; Determine if `val` is one of the legal vertical alignments
;;; ("top", "center', or "bottom").
(define valignment?
  (one-of "top" "center" "bottom"))

; +-------+----------------------------------------------------------
; | Above |
; +-------+

(sstruct %above %compound (halignment)
         #:cloneable
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (string-append (format "a ~a-aligned stack of images ("
                                     (%above-halignment img))
                             (let kernel ([remaining (subimages img)])
                               (if (null? (cdr remaining))
                                   (image-description (car remaining))
                                   (string-append (image-description (car remaining))
                                                  " above "
                                                  (kernel (cdr remaining)))))
                             ")")))]
         #:methods gen:img-make-pict
         [(define image-make-pict
            (lambda (img)
              (apply 2htdp:above/align
                     (cons (%above-halignment img)
                           (map image-picture (subimages img))))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (cons 'above/align
                    (cons (%above-halignment img)
                          (map image-structure (subimages img))))))]
         #:done)

;;; (above-halignment img) -> (one-of "left" "center" "right")
;;;   img : above?
;;; Determine the horizontal alignment of a stack of images.
(define above-halignment %above-halignment)

;;; (all-but-last lst) -> list?
;;;   lst : nonempty-list?
;;; Create a list with all the elements of lst but the last one.
(define all-but-last
  (lambda (lst)
    (if (null? (cdr lst))
        null
        (cons (car lst) (all-but-last (cdr lst))))))

;;; (above i1 i2 ... in [description]) -> image?
;;;   i1 : image?
;;;   i2 : image?
;;;   ...
;;;   in : image?
;;; Place the images above one another.
(define above
  (lambda images
    (when (null? images)
      (error 'above "expects at least two images, received none"))
    (let* ([tmp (last images)]
           [description (and (string? tmp) tmp)]
           [images (if description (all-but-last images) images)]
           [len (length images)])
      (when (< len 2)
        (error 'above "expects at least two images, received ~a" len))
      (let kernel ([remaining images])
        (when (not (null? remaining))
          (when (not (image? (car remaining)))
            (error 'above "expects images, received ~a" (car remaining)))
          (kernel (cdr remaining))))
      (%above description
              #f
              #f
              #f
              images
              "center"))))

;;; (above/align halignment i1 i2 ... in [description]) -> image?
;;;   halignment : horizontal-alignment?
;;;   i1 : image?
;;;   i2 : image?
;;;   ...
;;;   in : image?
;;;   description : string?
;;; Place the images above one another, aligned as described.
;;;
;;; halignment is either "left", "center", or "right".
(define above/align
  (lambda (halignment . images)
    (when (not (halignment? halignment))
      (error 'above/align "expects a horizontal alignment of \"left\", \"center\", or \"right\", received ~a" halignment))
    (when (null? images)
      (error 'above/align "expects at least two images, received none"))
    (let* ([tmp (last images)]
           [description (and (string? tmp) tmp)]
           [images (if description (all-but-last images) images)]
           [len (length images)])
      (when (< len 2)
        (error 'above/align "expects at least two images, received ~a" len))
      (let kernel ([remaining images])
        (when (not (null? remaining))
          (when (not (image? (car remaining)))
            (error 'above/align "expects images, received ~a" (car remaining)))
          (kernel (cdr remaining))))
      (%above description
              #f
              #f
              #f
              images
              halignment))))

; +--------+---------------------------------------------------------
; | Beside |
; +--------+

(sstruct %beside %compound (valignment)
         #:cloneable
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (string-append (format "a ~a-aligned sequence of images ("
                                     (%beside-valignment img))
                             (let kernel ([remaining (subimages img)])
                               (if (null? (cdr remaining))
                                   (image-description (car remaining))
                                   (string-append (image-description (car remaining))
                                                  " beside "
                                                  (kernel (cdr remaining)))))
                             ")")))]
         #:methods gen:img-make-pict
         [(define image-make-pict
            (lambda (img)
              (apply 2htdp:beside/align
                     (cons (%beside-valignment img)
                           (map image-picture (subimages img))))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (cons 'beside/align
                    (cons (%beside-valignment img)
                          (map image-structure (subimages img))))))]
         #:done)

;;; (beside-valignment img) -> valignment?
;;;   img : beside?
;;; Determine the vertical alignment of a sequence of images.
(define beside-valignment %beside-valignment)

;;; (beside i1 i2 ... in [description]) -> image?
;;;   i1 : image?
;;;   i2 : image?
;;;   ...
;;;   in : image?
;;;   description : string?
;;; Create an image with `i1` through `in` placed beside each other
;;; in a row.
(define beside
  (lambda images
    (when (null? images)
      (error 'beside "expects at least two images, received none"))
    (let* ([tmp (last images)]
           [description (and (string? tmp) tmp)]
           [images (if description (all-but-last images) images)]
           [len (length images)])
      (when (< len 2)
        (error 'beside "expects at least two images, received ~a" len))
      (let kernel ([remaining images])
        (when (not (null? remaining))
          (when (not (image? (car remaining)))
            (error 'beside "expects images, received ~a" (car remaining)))
          (kernel (cdr remaining))))
      (%beside description
               #f
               #f
               #f
               images
               "center"))))

;;; (beside/align valignment i1 i2 ... in [description]) -> image?
;;;   valignment : valignment?
;;;   i1 : image?
;;;   i2 : image?
;;;   ...
;;;   in : image?
;;;   description : string?
;;; Place `i1` through `in` in a row beside one another, aligned as
;;; described.
;;;
;;; valignment is either "top", "center", or "bottom".
(define beside/align
  (lambda (valignment . images)
    (when (not (valignment? valignment))
      (error 'beside/align "expects a vertical alignment of \"top\", \"center\", or \"bottom\", received ~a" valignment))
    (when (null? images)
      (error 'beside/align "expects at least two images, received none"))
    (let* ([tmp (last images)]
           [description (and (string? tmp) tmp)]
           [images (if description (all-but-last images) images)]
           [len (length images)])
      (when (< len 2)
        (error 'beside/align "expects at least two images, received ~a" len))
      (let kernel ([remaining images])
        (when (not (null? remaining))
          (when (not (image? (car remaining)))
            (error 'beside/align "expects images, received ~a" (car remaining)))
          (kernel (cdr remaining))))
      (%beside description
               #f
               #f
               #f
               images
               valignment))))

; +---------+--------------------------------------------------------
; | Overlay |
; +---------+

;;; (overlay-kernel images halignment valignment hoffset voffset) -> 2htdp:image?
;;;   images : (list-of image?)
;;;   halignment : (one-of "left" "center" "right")
;;;   valignment : (one-of "top" "center" "bottom")
;;;   hoffset : real?
;;;   voffset : real?
;;; Overlay a sequence of images according to the parameters.
(define overlay-kernel
  (lambda (images halignment valignment hoffset voffset)
    ; First overlay on transparent boxes (ignoring the offset).
    ; Then overlay with the offset.
    (let* ([w (apply max (map image-width images))]
           [h (apply max (map image-height images))]
           [bg (transparent-rectangle w h)])
      (let ([hfun (cond
                    [(equal? halignment "left")
                     (lambda (img) 0)]
                    [(equal? halignment "right")
                     (lambda (img) (- (image-width img) w))]
                    [else
                     (lambda (img) (* 1/2 (- (image-width img) w)))])]
            [vfun (cond
                    [(equal? valignment "top")
                     (lambda (img) 0)]
                    [(equal? valignment "bottom")
                     (lambda (img) (- (image-height img) h))]
                    [else
                     (lambda (img) (* 1/2 (- (image-height img) h)))])])
        (let ([intermediates
               (map (lambda (img)
                      (2htdp:overlay/xy (image-picture img)
                                        (hfun img)
                                        (vfun img)
                                        bg))
                    images)])
          ; (display intermediates)
          (let kernel ([remaining intermediates])
            (if (null? (cdr remaining))
                (car remaining)
                (2htdp:overlay/xy (car remaining)
                                  hoffset
                                  voffset
                                  (kernel (cdr remaining))))))))))


(sstruct %overlay %compound (halignment valignment hoffset voffset)
         #:cloneable
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (string-append (format "overlaid images, aligned ~a-~a ("
                                     (overlay-halignment img)
                                     (overlay-valignment img))
                             (let kernel ([remaining (subimages img)])
                               (if (null? (cdr remaining))
                                   (image-description (car remaining))
                                   (string-append (image-description (car remaining))
                                                  " over "
                                                  (kernel (cdr remaining)))))
                             ")")))]
         #:methods gen:img-make-pict
         [(define image-make-pict
            (lambda (img)
              (overlay-kernel (subimages img)
                              (overlay-halignment img)
                              (overlay-valignment img)
                              (overlay-hoffset img)
                              (overlay-voffset img))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (cons 'overlay/align
                    (cons (%overlay-halignment img)
                          (map image-structure (subimages img))))))]
         #:done)

(define overlay-halignment %overlay-halignment)
(define overlay-valignment %overlay-valignment)
(define overlay-hoffset %overlay-hoffset)
(define overlay-voffset %overlay-voffset)

;;; (overlay img1 ... imgn [description]) -> image?
;;;   img1 : image?
;;;   img2 : image?
;;;   ...
;;;   imgn : image?
;;;   description : string?
;;; Overlay `img1` through `imgn` on top of each other, keeping them centered
;;; on each other.
(define overlay
  (lambda images
    (when (null? images)
      (error 'overlay "expects at least two images, received none"))
    (let* ([tmp (last images)]
           [description (and (string? tmp) tmp)]
           [images (if description (all-but-last images) images)]
           [len (length images)])
      (when (< len 2)
        (error 'overlay "expects at least two images, received ~a" len))
      (let kernel ([remaining images])
        (when (not (null? remaining))
          (when (not (image? (car remaining)))
            (error 'overlay "expects images, received ~a" (car remaining)))
          (kernel (cdr remaining))))
      (%overlay description
                #f
                #f
                #f
                images
                "center"
                "center"
                0
                0))))

;;; (overlay/align halignment valignment i1 i2 ... in [description]) -> image?
;;;   halignment : horizontal-alignment?
;;;   valignment : vertical-alignment?
;;;   i1 : image?
;;;   i2 : image?
;;;   ...
;;;   in : image?
;;; Overlay the two images, aligning them as described.
;;;
;;; halignment is either "left", "center", or "right".
;;; valignment is either "top", "center", or "bottom".
(define overlay/align
  (lambda (halignment valignment . images)
    (when (not (halignment? halignment))
      (error 'overlay "expects a horizontal alignment of \"left\", \"center\", or \"right\", received ~a" halignment))
    (when (not (valignment? valignment))
      (error 'overlay "expects a vertical alignment of \"top\", \"center\", or \"bottom\", received ~a" valignment))
    (when (null? images)
      (error 'overlay/align "expects at least two images, received none"))
    (let* ([tmp (last images)]
           [description (and (string? tmp) tmp)]
           [images (if description (all-but-last images) images)]
           [len (length images)])
      (when (< len 2)
        (error 'overlay/align "expects at least two images, received ~a" len))
      (let kernel ([remaining images])
        (when (not (null? remaining))
          (when (not (image? (car remaining)))
            (error 'overlay/align "expects images, received ~a" (car remaining)))
          (kernel (cdr remaining))))
      (%overlay description
                #f
                #f
                #f
                images
                halignment
                valignment
                0
                0))))

; +-------+----------------------------------------------------------
; | Place |
; +-------+

(sstruct %place %compound (hside x vside y)
         #:cloneable
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              (let ([sub (subimages img)])
                (format "one image (~a) whose ~a is placed at ~a and whose ~a is placed at ~a on another image (~a)"
                        (image-description (car sub))
                        (%place-hside img)
                        (%place-x img)
                        (%place-vside img)
                        (%place-y img)
                        (image-description (cadr sub))))))]
         #:methods gen:img-make-pict
         [(define image-make-pict
            (lambda (img)
              (let ([sub (subimages img)])
                (2htdp:place-image/align (image-picture (car sub))
                                         (%place-x img)
                                         (%place-y img)
                                         (%place-hside img)
                                         (%place-vside img)
                                         (image-picture (cadr sub))))))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              (let ([sub (subimages img)])
                (list 'place/sides
                      (image-structure (car sub))
                      (%place-hside img)
                      (%place-x img)
                      (%place-vside img)
                      (%place-y img)
                      (image-structure (cadr sub))))))]
         #:done)

;;; (place/center img x y bg [description]) -> image?
;;;   img : image?
;;;   x : real?
;;;   y : real?
;;;   bg : image?
;;;   description : string?
;;; Place the center of `img` at position `(x,y)` on `bg`, cropping at
;;; the edges of `bg`.
(define place/center
  (lambda (img x y bg [description #f])
    (when (not (image? img))
      (error 'place/center
             "expects an image as the first parameter, received ~a"
             img))
    (when (not (real? x))
      (error 'place/center
             "expects a real number as the second parameter, received ~a"
             x))
    (when (not (real? y))
      (error 'place/center
             "expects a real number as the third parameter, received ~a"
             y))
    (when (not (image? bg))
      (error 'place/center
             "expects an image as the fourth parameter, received ~a"
             bg))
    (%place description
            #f
            #f
            #f
            (list img bg)
            "center"
            x
            "center"
            y)))

;;; (place img xside x yside y bg) -> image?
;;;   img : image?
;;;   xside : (one-of "left" "center" "right")
;;;   x : real?
;;;   yside : (one-of "top" "center" "bottom")
;;;   y : real?
;;;   bg : image?
;;;   description : string?
;;; Place `img` on `bg`, with the `xside` of `img` at `x` and the
;;; `yside` of `img` at `y`. Crop the result at the edges of `bg`.
(define place
  (lambda (img xside x yside y bg [description #f])
    (when (not (image? img))
      (error 'place
             "expects an image as the first parameter, received ~a"
             img))
    (when (not (member xside '("left" "center" "right")))
      (error 'place
             "expects left, center, or right as the second parameter, receved ~a"
             xside))
    (when (not (real? x))
      (error 'place "expects number as the third parameter, received ~a"
             x))
    (when (not (member yside '("top" "center" "bottom")))
      (error 'place
             "expects top, center, or bottom as the fourth parameter, receved ~a"
             yside))
    (when (not (real? y))
      (error 'place "expects a real number as the fifth parameter, received ~a"
             y))
    (when (not (image? bg))
      (error 'place "expects an image as the sixth parameter, received ~a"
             bg))
    (%place description
            #f
            #f
            #f
            (list img bg)
            xside
            x
            yside
            y)))

;;; (image-subtract img1 img2 [description]) -> image?
;;;   img1 : image?
;;;   img2 : image?
;;;   description : string?
;;; "Subtract" `img2` from `img1`, decreasing the opacity of each
;;; pixel in `img1` by the opacity of the corresponding pixel in
;;; `img2`. `image-subtract` does not affect the colors in `img1`.
(define image-subtract
  (lambda (img1 img2 [description #f])
    (let* ([w (image-width img1)]
           [h (image-height img2)]
           [bits1 (image-bitmap img1)]
           [tmp (2htdp:crop 0 0 w h
                            (2htdp:overlay/align "left" "top"
                                                 (image-picture img2)
                                                 (transparent-rectangle w h)))]
           [bits2 (image-bitmap (2htdp->bitmap tmp))]
           [result (bitmap w h description)]
           [bits (image-bitmap result)])
      (let kernel ([pos (- (vector-length bits) 1)])
        (when (>= pos 0)
          (let ([pixel1 (vector-ref bits1 pos)]
                [pixel2 (vector-ref bits2 pos)])
            (vector-set! bits pos
                         (rgb (rgb-red pixel1) (rgb-green pixel1) (rgb-blue pixel1)
                              (max 0 (- (rgb-alpha pixel1) (rgb-alpha pixel2)))))
            (kernel (- pos 1)))))
      result)))

; +------+-----------------------------------------------------------
; | Misc |
; +------+

;;; (basic-images img) -> (listof? image?)
;;;   img : image?
;;; Finds all of the basic images contained in img.
(define basic-images
  (lambda (img)
    (cond
      [(basic-image? img)
       (list img)]
      [(transformed? img)
       (basic-images (subimage img))]
      [(compound? img)
       (apply append (map basic-images (subimages img)))])))

;;; (image-map fun img) -> image?
;;;   fun : (image? -> image?)
;;;   img : image?
;;; Apply a function to each basic image in an image.
(define image-map
  (lambda (fun img)
    (cond
      [(compound-image? img)
       (let ([result (clone img)])
         (compound-set-images! result
                               (map (lambda (subimg)
                                      (image-map fun subimg))
                                    (subimages img)))
         result)]
      [(transformed-image? img)
       (let ([result (clone img)])
         (transformed-set-image! result (image-map fun (subimage img)))
         result)]
      [(basic-image? img)
       (fun img)]
      [else
       (error 'image-map "unknown image type ~a" img)])))

;;; (prepend-zeros str len) -> string?
;;;   str : string?
;;;   len : integer?
;;; Prepend zeros onto str so that the total length is len.
(define prepend-zeros
  (lambda (str len)
    (if (>= (string-length str) len)
        str
        (string-append (make-string (- len (string-length str)) #\0)
                       str))))

;;; (make-image-fname dir basename) -> string?
;;;   dir : string?
;;;   basename : string?
;;; Creates a name for the given image that does not overlap with
;;; a prior name.
(define make-image-fname
  (lambda (dir basename)
    (let kernel ([num 1])
      (let ([fname (string-append dir "/" basename
                                  (prepend-zeros (number->string num) 3)
                                  ".png")])
        ; (displayln fname)
        (if (file-exists? fname)
            (kernel (+ num 1))
            fname)))))

;;; (image-save img file) -> boolean?
;;;   img : image?
;;;   file : string?
;;; Save the image to the given file.
(define image-save
  (lambda (img file)
    (2htdp:save-image (image-picture img) file)))

;;; (markdown-image img dir) -> (void)
;;;   img : image?
;;;   dir : string?
;;; Save an image to the given directory and generate the
;;; appropriate markdown code.
(define markdown-image
  (lambda (img)
    (let ([dir (markdown-dir)])
      (when (and dir (not (directory-exists? dir)))
        (make-directory dir))
      (let* ([fname (image-fname img (or dir "."))])
        (2htdp:save-image (image-picture img) fname)
        (displayln (format "![~a](~a)" (image-description img) fname))))))

; +-------+----------------------------------------------------------
; | Notes |
; +-------+

;;; (cat color) -> image?
;;;   color : color?
;;; Create an image that might resemble a cat, primarily for testing 
;;; purposes.
(define cat
  (lambda (color)
    (solid-polygon (list (pt 0 0) (pt 100 0) (pt 50 20) (pt 100 100)
                         (pt 0 100) (pt 50 80) (pt 50 20))
                   color)))

#|
|#

#|
         #:methods gen:img-make-desc
         [(define image-make-desc
            (lambda (img)
              XXX))]
         #:methods gen:img-make-pict
         [(define image-make-pict
            (lambda (img)
              XXX))]
         #:methods gen:img-make-stru
         [(define image-make-stru
            (lambda (img)
              XXX))]
         #:done)
|#
