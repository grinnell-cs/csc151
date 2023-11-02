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

(define bitmap-force-image!
  (lambda (bitmap)
    (when (not (bitmap-image bitmap))
      (set-bitmap-image! bitmap (color-list->bitmap (vector->list (bitmap-pixels bitmap))
                                                    (bitmap-width bitmap)
                                                    (bitmap-height bitmap))))))

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

(struct bitmap (width height pixels image)
  #:mutable
  #:methods gen:custom-write
  [(define write-proc bitmap-print)])

(define bitmap->image
  (lambda (bitmap)
    (bitmap-force-image! bitmap)
    (bitmap-image bitmap)))

(define image->bitmap
  (lambda (image)
    (bitmap (image-width image)
            (image-height image)
            (list->vector (image->color-list image))
            image)))

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
Old:
> (time (create-bitmap (lambda (x y) (color 0 0 y)) 255 255))
cpu time: 92 real time: 134 gc time: 49
New:
> (time (create-bitmap (lambda (x y) (color 0 0 y)) 255 255))
cpu time: 46 real time: 49 gc time: 23
|#

