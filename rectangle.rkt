#lang racket

;;; File:
;;;   rectangle.rkt
;;; Summary:
;;;   A variety of procedures for working with rectangles.
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
(require "polygon.rkt")

(provide (all-defined-out))

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
               (if (precise-polygon-colors)
                   (color->hex (image-color img))
                   (color->color-name (image-color img)))
               (round (rectangle-width img))
               (round (rectangle-height img)))))]
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

; +------------------+-----------------------------------------------
; | Solid rectangles |
; +------------------+

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
    (param-check! solid-rectangle 1 nonnegative-real? width)
    (param-check! solid-rectangle 2 nonnegative-real? height)
    (param-check! solid-rectangle 3 color? color)
    (when description
      (param-check! solid-rectangle 4 string? description))
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

; +---------------------+--------------------------------------------
; | Outlined rectangles |
; +---------------------+

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
    (param-check! outlined-rectangle 1 nonnegative-real? width)
    (param-check! outlined-rectangle 2 nonnegative-real? height)
    (param-check! outlined-rectangle 3 color? color)
    (param-check! outlined-rectangle 4 positive-real? line-width)
    (when description
      (param-check! outlined-rectangle 5 string? description))
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

; +---------------------------+--------------------------------------
; | General rectangle methods |
; +---------------------------+

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

; +---------------+--------------------------------------------------
; | Miscellaneous |
; +---------------+

;;; (transparent-rectangle width height) -> image?
;;;   width : non-negative-integer?
;;;   height : non-negative-integer?
;;; Make a transparent rectangle of the given size.
(define transparent-rectangle
  (lambda (width height)
    (solid-rectangle width height (rgb 0 0 0 0))))

