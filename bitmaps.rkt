#lang racket
(require 2htdp/image)

(provide bitmap?
         create-bitmap
         bitmap->image
         image->bitmap
         bitmap-pixel
         bitmap-set-pixel!
         bitmap-width
         bitmap-height)

; +-----------------+------------------------------------------------
; | Local Utilities |
; +-----------------+

;;; (bitmap-force-image! bitmap) -> (void)
;;;   bitmap : bitmap?
;;; Force the creation of the image field in a bitmap.
(define bitmap-force-image!
  (lambda (bitmap)
    (when (not (bitmap-image bitmap))
      (set-bitmap-image! bitmap (color-list->bitmap (vector->list (bitmap-pixels bitmap))
                                                    (bitmap-width bitmap)
                                                    (bitmap-height bitmap))))))

;;; (bitmap-print bitmap port mode) -> (void)
;;;   bitmap : bitmap?
;;;   print : port?
;;;   mode : print-mode? (#t, #f, 0, or 1)
;;; Print the bitmap to the specified port in the specified manner.
;;; Note: Needs to be defined before the bitmap struct.
(define bitmap-print
  (lambda (bitmap port mode)
    (bitmap-force-image! bitmap)
    (cond
      [(eq? mode true)
       (write (bitmap-image bitmap) port)]
      [(eq? mode false)
       (display (bitmap-image bitmap) port)]
      [else
       (print (bitmap-image bitmap) port mode)])))

; +---------+--------------------------------------------------------
; | Structs |
; +---------+

(struct bitmap (width height pixels image)
  #:mutable
  #:methods gen:custom-write
  [(define write-proc bitmap-print)])

; +---------------------+--------------------------------------------
; | Exported procedures |
; +---------------------+

;;; (bitmap->image bitmap) -> image?
;;;   bitmap : bitmap?
;;; Convert a bitmap into an image.  (E.g., so that we can rotate,
;;; overlay, scale, etc.)
(define bitmap->image
  (lambda (bitmap)
    (bitmap-force-image! bitmap)
    (bitmap-image bitmap)))

;;; (image->bitmap image) -> bitmap?
;;;   image : image?
;;; Convert an image into a bitmap.  (E.g., so that we can get and
;;; set pixels.)
(define image->bitmap
  (lambda (image)
    (bitmap (image-width image)
            (image-height image)
            (list->vector (image->color-list image))
            image)))

;;; (bitmap-pixel bitmap col row) -> color?
;;;   bitmap : bitmap?
;;;   col : non-negative-integer? (less than (bitmap-width bitmap))
;;;   row : non-negative-integer? (less than (bitmap-height bitmap))
;;; Get the color of the pixel at position (`col`,`row`) in `bitmap`.
(define bitmap-pixel
  (lambda (bitmap col row)
    (cond
      [(not (exact-integer? col))
       (error "bitmap-pixel: column must be an exact integer")]
      [(not (exact-integer? row))
       (error "bitmap-pixel: row must be an exact integer")]
      [(or (>= col (bitmap-width bitmap)) (< col 0))
       (error "bitmap-pixel: Invalid column" col)]
      [(or (>= row (bitmap-height bitmap)) (< row 0))
       (error "bitmap-pixel: Invalid row" row)]
      [else
       (vector-ref (bitmap-pixels bitmap)
                   (+ (* row (bitmap-width bitmap))
                      col))])))

;;; (bitmap-set-pixel! bitmap col row color) -> void?
;;;   bitmap : bitmap?
;;;   col : non-negative-integer? (less than (bitmap-width bitmap))
;;;   row : non-negative-integer? (less than (bitmap-height bitmap))
;;;   color : color?
;;; Set the color of the pixel at position (`col`,`row`) in `bitmap`.
(define bitmap-set-pixel!
  (lambda (bitmap col row color)
    (cond
      [(not (exact-integer? col))
       (error "bitmap-set-pixel!: column must be an exact integer")]
      [(not (exact-integer? row))
       (error "bitmap-set-pixel!: row must be an exact integer")]
      [(not (color? color))
       (error "bitmap-set-pixel!: invalid color" color)]
      [(or (>= col (bitmap-width bitmap)) (< col 0))
       (error "bitmap-set-pixel!: Invalid column" col)]
      [(or (>= row (bitmap-height bitmap)) (< row 0))
       (error "Invalid row: Outside of range")]
      [else
       (vector-set! (bitmap-pixels bitmap)
                    (+ (* row (bitmap-width bitmap))
                       col)
                    color)])))
    
;;; (create-bitmap func width height) -> bitmap?
;;;   func : a procedure that takes two non-negative integers as input
;;;          and returns a color
;;;   width : positive-integer?
;;;   height : positive-integer?
;;; Creates a new `width`-by-`height` bitmap by applying `func` to
;;; each (col,row) coordinate to determine the color at that position.
(define create-bitmap
  (lambda (fun width height)
    (let ([pixels (make-vector (* width height))])
      (let kernel ([pos 0]
                   [col 0]
                   [row 0])
        (cond
          [(>= col width)
           (kernel pos 0 (+ row 1))]
          [(< row height)
           (vector-set! pixels
                        pos
                        (fun col row))
           (kernel (+ pos 1) (+ col 1) row)])
        (bitmap width height pixels false)))))

#|
Create bitmap originally lacked the pos parameter in the kernel.
Here's a quick experiment of before and after adding it.

Old:
> (time (create-bitmap (lambda (x y) (color 0 0 y)) 255 255))
cpu time: 92 real time: 134 gc time: 49
New:
> (time (create-bitmap (lambda (x y) (color 0 0 y)) 255 255))
cpu time: 46 real time: 49 gc time: 23
|#

