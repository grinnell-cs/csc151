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

(provide (all-defined-out))

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
  (image-color img-color)
  #:fallbacks
  [(define image-color
     (lambda (img)
       #f))])

;;; (subimages img) -> (list-of image?)
;;;   img : image?
;;; Get a list of all the subimages of the current image.
(define-generics img-subimages
  (subimages img-subimages)
  #:fallbacks
  [(define subimages
     (lambda (img)
       null))])

;;; (outlined? img) -> boolean?
;;;   img : image?
;;; Determine if an image is outlined.
(define-generics outlined)

;;; (solid? img) -> boolean?
;;;   img : image?
;;; Determine if an image is solid.
(define-generics solid)

;;; (horizontally-flipped? img) -> boolean?
;;;   img : image?
;;; Determine if an image was built by horizontally flipping another image.
(define-generics horizontally-flipped)

;;; (vertically-flipped? img) -> boolean?
;;;   img : image?
;;; Determine if an image was built by vertically flipping another image.
(define-generics vertically-flipped)

; +--------+---------------------------------------------------------
; | Points |
; +--------+

;;; (pt x y) -> pt?
;;;   x : real?
;;;   y : real?
;;; Make a point.
(struct pt (x y)
  #:transparent)

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

;;; (angle-between-points a b c) -> real?
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

; +----------------+-------------------------------------------------
; | Generic images |
; +----------------+

;;; (image [description] [structure] [bits] [picture]) -> image?
;;;   description : string?
;;;   structure : list?
;;;   bits : (vector-of rgb?)
;;;   picture : 2htdp:image?
;;; Our generic image.
(struct %image 
  ([desc #:mutable]
   [stru #:mutable]
   [bits #:mutable]
   [pict #:mutable])
  #:transparent
  #:methods gen:img-make-pict []
  #:methods gen:img-make-bits []
  #:methods gen:img-make-stru []
  #:methods gen:img-make-desc []
  #:methods gen:img-color []
  #:methods gen:img-subimages []
  #:methods gen:custom-write
  [(define write-proc
     (lambda (img port mode)
       (write (image-picture img) port)))]
  )

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
    (2htdp:image-height (image-picture img))))

;;; (image-width img) -> exact-integer?
;;;   img : image?
;;; Get the width of the image.
(define image-width
  (lambda (img)
    (2htdp:image-width (image-picture img))))

; +--------------+---------------------------------------------------
; | Basic images |
; +--------------+

;;; (basic-image [color] [description] [structure] [bits] [pict]) -> image?
;;;   color : color?
;;;   description : string?
;;;   structure : list?
;;;   bits : (vector-of rgb?)
;;;   picture : 2htdp:image?
;;; A basic image; intended primarily as a placeholder in the hierarchy.
(struct %basic-image %image (color)
  #:transparent
  #:reflection-name 'basic-image
  #:methods gen:img-color
  [(define image-color
     (lambda (img)
       (%basic-image-color img)))])

(define basic-image? %basic-image?)
(define basic-image-color %basic-image-color)

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
(struct %shape %basic-image ())

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

(struct %polygon %shape (points))

(define polygon? %polygon?)
(define polygon-points %polygon-points)

(struct %solid-polygon %polygon ()
  #:methods gen:solid [])

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
      (%solid-polygon (if description
                          description
                          (format "a solid ~a polygon built from the points ~e"
                                  (rgb->color-name color)
                                  points))
                      (list 'solid-polygon points color)
                      #f
                      (2htdp:polygon (map pt->posn points) "solid" 
                                     (rgb->2htdp color))
                      color
                      points))))

;;; (polygon-sides poly) -> non-negative-integer?
;;;   poly : polygon?
;;; Determine the number of sides in the polygon.
(define polygon-sides
  (lambda (poly)
    (length (polygon-points poly))))

;;; (polygon-side poly n) -> real?
;;;   poly : polygon?
;;;   n : (all-of? non-negative-integer? (cut (< <> (polygon-sides poly))))
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

; +------------+-----------------------------------------------------
; | Rectangles |
; +------------+

(struct %solid-rectangle %solid-polygon (width height))

(define solid-rectangle? %solid-rectangle?)
(define solid-rectangle-width  %solid-rectangle-width)
(define solid-rectangle-height %solid-rectangle-height)

;;; (solid-rectangle width height color [desc]) -> image?
;;;   width : non-negative-real?
;;;   height : non-negative-real?
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
      (%solid-rectangle (if description
                            description
                            (format "a solid ~a ~a-by-~a rectangle"
                                    (rgb->color-name color)
                                    width height))
                        (list 'rectangle width height color)
                        #f
                        (2htdp:polygon (map pt->posn points) "solid" 
                                       (rgb->2htdp color))
                        color
                        points
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
        (and (polygon? img)
             (rectangular-polygon? img))
        (and (transformed? img)
             (rectangle? (subimage img))))))

;;; (rectangle-height rect) -> real?
;;;   rect : rectangle?
;;; Determine the height of a rectangle.
(define rectangle-height
  (lambda (rect)
    (cond
      [(solid-rectangle? rect)
       (solid-rectangle-height rect)]
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
      [(and (polygon? rect) (rectangular-polygon? rect))
       (let ([points (polygon-points rect)])
         (distance (car points) (cadr points)))]
      [(transformed? rect)
       (rectangle-width (car (subimages rect)))]
      [else
       (error 'rectangle-width "expected a rectangle, received ~e" rect)])))

; +---------+--------------------------------------------------------
; | Squares |
; +---------+

(struct %solid-square %solid-rectangle ())

(define solid-square? %solid-square?)

;;; (solid-square side color [description]) -> image?
;;;   side : non-negative-integer?
;;;   color : color?
;;;   description : string?
;;; Create a solid square with the given side length and color.
(define solid-square
  (lambda (side color [description #f])
    (let ([color (color->rgb color)]
          [points (list (pt 0 0) (pt side 0) (pt side side) (pt 0 side))])
      (%solid-square (if description
                         description
                         (format "a solid ~e square with side length ~e"
                                 (rgb->color-name color)
                                 side))
                     (list 'square side color)
                     #f
                     (2htdp:polygon (map pt->posn points) "solid" 
                            (rgb->2htdp color))
                     color
                     points
                     side
                     side))))

;;; (square? img) -> boolean?
;;;   img : image?
;;; Determine if the image is a square.
(define square?
  (lambda (img)
    (or (solid-square? img)
        (and (rectangle? img)
             (<= (abs (- (rectangle-width img) (rectangle-height img))) 0.01))
        (and (transformed? img))
             (square? (subimage img)))))

;;; (square-side sq) -> real?
;;;   sq : square?
;;; Determine the side length of a square.
(define square-side
  (lambda (sq)
    (rectangle-width sq)))

; +----------+-------------------------------------------------------
; | Diamonds |
; +----------+

(struct %solid-diamond %solid-polygon (width height))

(define solid-diamond? %solid-diamond?)
(define solid-diamond-width  %solid-diamond-width)
(define solid-diamond-height %solid-diamond-height)

;;; (solid-diamond width height color [desc]) -> image?
;;;   width : non-negative-real?
;;;   height : non-negative-real?
;;;   color : color?
;;;   description : string?
;;; A polygon whose vertices are given by `points` and whose color is `color`.
(define solid-diamond
  (lambda (width
           height
           color 
           [description #f])
    (let* ([color (color->rgb color)]
           [w (/ width 2)]
           [h (/ height 2)]
           [points (list (pt w 0) (pt width h) (pt w height) (pt 0 h))])
      (%solid-diamond (if description
                          description
                          (format "a solid ~e ~e-by-~e diamond"
                                  (rgb->color-name color)
                                  width height))
                      (list 'diamond width height color)
                      #f
                      (2htdp:polygon (map pt->posn points) "solid" 
                                     (rgb->2htdp color))
                      color
                      points
                      width
                      height))))

; +----------+-------------------------------------------------------
; | Ellipses |
; +----------+

(struct %ellipse %shape (width height))

(struct %solid-ellipse %ellipse ()
  #:methods gen:solid [])

(define solid-ellipse? %solid-ellipse?)

;;; (solid-ellipse width height color [description]) -> image?
;;;   width : non-negative-real?
;;;   height : non-negative-real?
;;;   color : color?
;;;   description : string?
;;; A `width`-by-`height` ellipse whose color is `color`.
(define solid-ellipse
  (lambda (width
           height
           color 
           [description #f])
    (let ([color (color->rgb color)])
      (%solid-ellipse (if description
                          description
                          (format "a solid ~a ~a-by-~a ellipse"
                                  (rgb->color-name color)
                                  width
                                  height))
                      (list 'solid-ellipse width height color)
                      #f
                      (2htdp:ellipse width height "solid" 
                                     (rgb->2htdp color))
                      color
                      width
                      height))))

;;; (ellipse? img) -> boolean?
;;;   img : image?
;;; Determine if an image is an ellipse.
(define ellipse?
  (lambda (img)
    (or (solid-ellipse? img)
        (and (transformed? img)
             (ellipse? (subimage img))))))

; +-----------------+------------------------------------------------
; | Transformations |
; +-----------------+

(struct %transformed %image (subimg)
  #:transparent
  #:methods gen:img-subimages
  [(define subimages
     (lambda (img)
       (list (%transformed-subimg img))))])

(define subimage %transformed-subimg)

(define transformed? %transformed?)

(struct %rotated %transformed (angle)
  #:transparent)

(define rotated? %rotated?)

;;; (rotate img angle [description]) -> image?
;;;   img : image?
;;;   angle : real?
;;;   description : string?
;;; Create a new image by rotating `img` by `angle` degrees
;;; counter-clockwise.
(define rotate
  (lambda (img angle [description #f])
    (%rotated (if description
                  description
                  (format "~a rotated by ~a degrees"
                           (image-description img)
                           angle))
              (list 'rotate (image-structure img) angle)
              #f
              (2htdp:rotate angle (image-picture img))
              img
              angle)))

; +--------------+---------------------------------------------------
; | Combinations |
; +--------------+
