#lang racket

;;; File:
;;;   image.rkt
;;; Summary:
;;;   A variety of procedures for working with images.
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

(provide (all-defined-out))

; +----------+-------------------------------------------------------
; | Settings |
; +----------+

;;; (markdown-dir [dir]) -> string?
;;;   dir : (any-of string? false?)
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

;;; (transparent-2htdp-rectangle width height) -> 2htdp:image?
;;;   width : nonnegative-integer?
;;;   height : nonnegative-integer?
;; Make a width-by-height transparent rectangle.
(define transparent-2htdp-rectangle
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
                    (let* ([bg (transparent-2htdp-rectangle propw height)]
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
                    (let* ([bg (transparent-2htdp-rectangle width proph)]
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
                              (transparent-2htdp-rectangle width height))))))

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
      [(member mode '("outline" outline "outlined" outlined))
       ; (displayln (format "Creating an outlined ~a" name))
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
                     (round (* 1/255 mode (rgb-alpha tmp))))))]
      [else
       (error name "invalid mode: ~a" mode)])))

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
       ; (displayln (list 'write-proc 'img port mode))
       (when (markdown-dir)
         (when (not (equal? (format "~a" port) "#<output-port:null>"))
           (markdown-image img)))
       (write (image-picture img) port)))]
  #:methods gen:equal+hash
  [(define equal-proc 
     (lambda (a b equal?-recur)
       (equal?-recur (image-picture a) (image-picture b))))
   (define hash-proc 
     (lambda (a hash-recur)
       (hash-recur (image-picture a))))
   (define hash2-proc 
     (lambda (a hash2-recur)
       (hash2-recur (image-picture a))))]
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

;;; (image-get-pixel image col row) -> rgb?
;;;   image : image?
;;;   col : (all-of nonnegative-integer? (less-than (bitmap-width image)))
;;;   row : (all-of nonnegative-integer? (less-than (bitmap-height image)))
;;; Get the color of the pixel at position (`col`,`row`) in `image`.
(define image-get-pixel/kernel
  (lambda (image col row)
    (vector-ref (image-bitmap image)
                (+ col (* row (image-width image))))))

(define image-get-pixel
  (lambda (image col row)
    (param-check! image-get-pixel 1 image? image)
    (param-check! image-get-pixel 2 nonnegative-integer? col)
    (param-check! image-get-pixel 2 (less-than (image-width image)) col)
    (param-check! image-get-pixel 3 nonnegative-integer? row)
    (param-check! image-get-pixel 3 (less-than (image-height image)) row)
    (image-get-pixel/kernel image col row)))

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

;;; (bitmap width height description) -> image?
;;;   width : positive-integer?
;;;   height : positive-integer?
;;;   description : string?
;;; Creates a new `width`-by-`height` transparent bitmap.
(define bitmap
  (lambda (width height description)
    (param-check! bitmap 1 positive-integer? width)
    (param-check! bitmap 2 positive-integer? height)
    (param-check! bitmap 3 string? description)
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

;;; (image-load fname [description]) -> bitmap?
;;;   fname : string?
;;;   description : string?
;;; Load the given image.
(define image-load
  (lambda (fname [description #f])
    (let ([img (2htdp:bitmap/file fname)])
      (%bitmap description
               (list 'image-load fname)
               (list->vector (map 2htdp->rgb (2htdp:image->color-list img)))
               img
               (2htdp:image-width img)
               (2htdp:image-height img)))))

; +------------------------------+-----------------------------------
; | Additional bitmap operations |
; +------------------------------+

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

; +------+-----------------------------------------------------------
; | Misc |
; +------+

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

; +------------------+-----------------------------------------------
; | Pixel procedures |
; +------------------+

;;; (pixel-map color-transformation img [description]) -> image?
;;;   color-transformation : procedure?
;;;   img : image?
;;;   description : string?
;;; Create a new image by applying color-transformation to each pixel
;;; in the original image.
(define pixel-map
  (lambda (ctrans img [description #f])
    (let* ([w (image-width img)]
           [h (image-height img)]
           [orig (image-bitmap img)]
           [result (bitmap w h (or description
                                   (format "a transformed version of ~a"
                                           (image-description img))))]
           [bits (image-bitmap result)])
      (let kernel ([pos (- (vector-length bits) 1)])
        (when (>= pos 0)
          (vector-set! bits pos (ctrans (vector-ref orig pos)))
          (kernel (- pos 1))))
      result)))

;;; (pixel-plus-map make-color img [description]) -> image?
;;;   make-color : procedure?
;;;   img : img?
;;;   description : string?
;;; Create a new image by applying `transformation` at each position
;;; in the image.
;;;
;;; `transformation` should take the following parameters:
;;;   pixel : rgb? (corresponding to the color of the current pixel)
;;;   col : integer? (corresponding to the column of the current pixel)
;;;   row : integer? (corresponding to the row of the current pixel)
;;;   northwest : rgb? (corresponding to the color of the pixel up one row and left one column)
;;;   north : rgb? (corresponding to the color of the pixel up one row and in the same column)
;;;   northeast : rgb? (corresponding to the color of the pixel up one row and right one column)
;;;   west : rgb? (corresponding to the color of the pixel immediately left of the pixel)
;;;   east : rgb? (corresponding to the color of the pixel immediately right of the piel)
;;;   southwest : rgb? (corresponding to the color of the pixel down one row and left one column)
;;;   south :rgb? (corresponding to the color of the pixel immediately below the pixel)
;;;   southeast : rgb? (corresponding to the color of the pixel down one row and right one column)
;;;
;;; Boundary cells get (rgb 128 128 128) for appropriate neighbors.
(define pixel-plus-map
  (lambda (make-color img [description #f])
    (let* ([default (rgb 128 128 128)]
           [w (image-width img)]
           [h (image-height img)]
           [orig (image-bitmap img)]
           [result (bitmap w h (or description
                                   (format "a transformed version of ~a"
                                           (image-description img))))]
           [bits (image-bitmap result)])
      (let kernel ([col 0]
                   [row 0]
                   [pos 0])
        (when (< row h)
          (cond
            [(>= col w)
             (kernel 0 (+ row 1) pos)]
            [else
             (let* ([lastcol? (>= col (- w 1))]
                    [lastrow? (>= row (- h 1))]
                    [northwest (if (or (zero? row) (zero? col)) 
                                  default
                                  (vector-ref bits (- pos 1)))]
                    [north (if (zero? row)
                               default
                               (vector-ref bits (- pos w)))]
                    [northeast (if (or (zero? row) lastcol?)
                                   default
                                   (vector-ref bits (+ 1 (- pos w))))]
                    [west (if (zero? col)
                              default
                              (vector-ref bits (- pos 1)))]
                    [east (if (>= col (- w 1))
                              default
                              (vector-ref bits (+ pos 1)))]
                    [southwest (if (or lastrow? (zero? col))
                                   default
                                   (vector-ref bits (+ pos w -1)))]
                    [south (if lastrow?
                               default
                               (vector-ref bits (+ pos w)))]
                    [southeast (if (or lastrow? lastcol?)
                                   default
                                   (vector-ref bits (+ pos w 1)))])
               (vector-set! bits 
                            pos 
                            (make-color (vector-ref bits pos)
                                        col row
                                        northwest north northeast
                                        west east
                                        southwest south southeast))
               (kernel (+ col 1) row (+ pos 1)))])))
      result)))

