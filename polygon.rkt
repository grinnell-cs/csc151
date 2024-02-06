#lang racket

;;; File:
;;;   polygon.rkt
;;; Summary:
;;;   A variety of procedures for working with polygons.
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
(require (only-in "combinations.rkt" place))

(provide (all-defined-out))

; +----------------+-------------------------------------------------
; | Polygon basics |
; +----------------+

(sstruct %polygon %shape (points))

;;; (polygon? img) -> boolean?
;;;   image : image?
;;; Determines whether `img` is a polygon.
(define polygon? %polygon?)

;;; (polygon-points poly) -> (list-of pt?)
;;;   poly : polygon?
;;; Grab the points of a polygon.
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
        (error 'polygon-side
               "Expected a number between 0 (inclusive) and ~a (exclusive), received ~e"
               sides n))
      (if (= n (- sides 1))
          (distance (car points) (list-ref points n))
          (distance (list-ref points n) (list-ref points (+ n 1)))))))

;;; (sides-similar? poly i j) -> boolean?
;;;   poly : polygon?
;;;   i : (all-of nonnegative-integer? (less-then (polygon-sides poly)))
;;;   j : (all-of nonnegative-integer? (less-then (polygon-sides poly)))
;;; Determine whether sides i and j are a similar length.
(define sides-similar
  (lambda (poly i j)
    (param-check! side-similar 0 polygon? poly)
    (<= (abs (- (polygon-side poly i)
                (polygon-side poly j)))
        1)))

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

; +----------------+-------------------------------------------------
; | Solid polygons |
; +----------------+

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
;;; Create polygon whose vertices are given by `points` and whose
;;; color is `color`.
;;;
;;; Warning! The edges of the polygon should not cross. In such cases,
;;; the results are unpredictable.
(define solid-polygon
  (lambda (points color [description #f])
    (param-check! solid-polygon 1 (list-of pt?) points)
    (param-check! solid-polygon 2 color? color)
    (when description
      (param-check! solid-polygon 3 string? description))
    (let ([color (color->rgb color)])
      (%solid-polygon description #f #f #f color points))))

; +-------------------+----------------------------------------------
; | Outlined polygons |
; +-------------------+

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

;;; (outlined-polygon points color pen-width [description]) -> image?
;;;   points : (list-of point?)
;;;   color : color?
;;;   line-width : positive-integer?
;;;   description : string?
;;; Create a polygon whose vertices are given by `points` and which
;;; is outlined by a `color` line whose width is given by `line-width`.
;;;
;;; Note: Although the points in the polygon may not describe a shape
;;; whose left edge is 0 and whose top edge is 0, `outlined-polygon` will
;;; shift the polygon so that the left is 0 and the top is 0.
;;;
;;; Warning! The edges of the polygon should not cross. In such cases,
;;; the results are unpredictable.
;;;
;;; Warning! Due to some infelicities in the design of `outlined-polygon`,
;;; it does not always work correctly for non-convex polygons
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

(sstruct %solid-coordinate-polygon %polygon ()
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
       (let* ([points (polygon-points img)]
              [xcoords (map pt-x points)]
              [ycoords (map pt-y points)]
              [left (apply min xcoords)]
              [top (apply min ycoords)]
              [width (apply max xcoords)]
              [height (apply max ycoords)])
         (displayln (format "left: ~a, top: ~a, width: ~a, height: ~a"
                            left top width height))
         (2htdp:place-image/align
          (2htdp:polygon (map pt->posn (polygon-points img))
                         "solid"
                         (color->2htdp (image-color img)))
          left top
          "left" "top"
          (transparent-rectangle width height)))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'solid-coordinate-polygon (polygon-points img) (image-color img))))]
  #:done)

; +---------------------------+--------------------------------------
; | Solid coordinate polygons |
; +---------------------------+

(define solid-coordinate-polygon? %solid-coordinate-polygon?)

;;; (solid-coordinate-polygon points color [description]) -> image?
;;;   points : list-of (pt?)
;;;   color : color?
;;;   description : string?
;;; Create a polygon from the specified points. Unlike `solid-polygon`,
;;; `solid-coordinate-polygon` does not shift the polygon. Hence, if
;;; coordinates are negative, portions of the polygon may be cut off.
;;; Similarly, if coordinates are all positive, there may be some
;;; blank space to the left of the polygon.
(define solid-coordinate-polygon
  (lambda (points color [description #f])
    (param-check! solid-coordinate-polygon 1 (list-of pt?) points)
    (param-check! solid-coordinate-polygon 2 color? color)
    (when description
      (param-check! solid-coordinate-polygon 3 string? description))
    (let ([color (color->rgb color)])
      (%solid-coordinate-polygon description #f #f #f color points))))

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

; +-------------------+----------------------------------------------
; | Polygon utilities |
; +-------------------+

;;; (expanded-polygon-line pt1 pt2 distance) -> (any-of line? real?)
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

;;; (polygon-lines points) -> (list-of line?)
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

; +---------------------+--------------------------------------------
; | Connecting the dots |
; +---------------------+

;;; (add-lines lines line-color img) -> 2htdp:image?
;;;   lines : (list-of (pair-of pt? pt?))
;;;   line-color : 2htdp:color?
;;;   img : 2htdp:image?
;;; Add the lines between the specified points to the images.
(define add-lines
  (lambda (lines line-color img)
    (param-check! add-lines 1 (list-of (pair-of pt? pt?)) lines)
    (param-check! add-lines 2 2htdp:color? line-color)
    (param-check! add-lines 3 2htdp:image? img)
    (add-lines/kernel lines line-color img)))

(define add-lines/kernel
  (lambda (lines line-color img)
    (let kernel ([img img]
                 [lines lines])
      (if (null? lines)
          img
          (let ([pt1 (caar lines)]
                [pt2 (cdar lines)])
            (kernel (2htdp:scene+line img
                                      (pt-x pt1) (pt-y pt1)
                                      (pt-x pt2) (pt-y pt2)
                                      line-color)
                    (cdr lines)))))))

;;; (place-dots dot points img) -> 2htdp:image?
;;;   dot : 2htdp:image?
;;;   points : (list-of pt?)
;;;   img : 2htdp:image?
;;; Place a copy of `dot` at each point in `points` in `img`.
(define place-dots
  (lambda (dot points img)
    (param-check! place-dots 1 2htdp:image? dot)
    (param-check! place-dots 2 (list-of pt?) points)
    (param-check! place-dots 3 2htdp:image? img)
    (let kernel ([img img]
                 [points points])
      (if (null? points)
          img
          (kernel (2htdp:place-image dot
                                     (pt-x (car points))
                                     (pt-y (car points))
                                     img)
                  (cdr points))))))

;;; (connect-the-dots points line-color dot-color [description]) -> image?
;;;   points : (list-of pt?)
;;;   line-color : color?
;;;   dot-color : color?
;;;   description : string?
;;; Make an image by drawing lines between the points and then
;;; putting small dots at each point.
(define connect-the-dots
  (let ([dot-size 6])
    (lambda (points line-color dot-color [description #f])
      (param-check! connect-the-dots 1 (list-of pt?) points)
      (param-check! connect-the-dots 2 color? line-color)
      (param-check! connect-the-dots 3 color? dot-color)
      (when description
        (param-check! connect-the-dots 4 string? description))
      (let* ([dot (2htdp:circle dot-size "solid" (color->2htdp dot-color))]
             [width (+ dot-size (apply max (map pt-x points)))]
             [height (+ dot-size (apply max (map pt-y points)))]
             [bg (2htdp:rectangle width height "solid" (2htdp:color 0 0 0 0))])
        (place-dots dot points
                    (add-lines (points->lines (lastfirst points))
                               (color->2htdp line-color)
                               bg))))))

;;; (points->lines points) -> (list-of (pair-of pt? pt?))
;;;   points : (list-of pt?)
;;; Turn each pair of subsequent points in points into a
;;; pair of points.
(define points->lines
  (lambda (points)
    (if (null? (cdr points))
        null
        (cons (cons (car points) (cadr points))
              (points->lines (cdr points))))))

;;; (lines-from source points) -> (list-of (pair-of pt? pt?))
;;;   source : pt?
;;;   points? : (list-of pt?)
;;; Create a list of lines from `source` to each point in `points`.
(define lines-from
  (lambda (source points)
    (map (lambda (point)
           (cons source point))
         points)))

; +----------------+-------------------------------------------------
; | Misc utilities |
; +----------------+

;;; (lastfirst lst) -> list?
;;;   lst : list?
;;; Add the last element in `lst` to the front of the list.
(define lastfirst
  (lambda (lst)
    (cons (last lst) lst)))

