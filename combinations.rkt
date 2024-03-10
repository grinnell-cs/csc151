#lang racket

;;; File:
;;;   combinations.rkt
;;; Summary:
;;;   A variety of procedures for combining images.
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
           [bg (transparent-2htdp-rectangle w h)])
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
;;; `img2`. `image-subtract` does not otherwise affect the colors
;;; in `img1`.
(define image-subtract
  (lambda (img1 img2 [description #f])
    (let* ([w (image-width img1)]
           [h (image-height img1)]
           [bits1 (image-bitmap img1)]
           [tmp (2htdp:crop 0 0 w h
                            (2htdp:overlay/align "left" "top"
                                                 (image-picture img2)
                                                 (transparent-2htdp-rectangle w h)))]
           [bits2 (image-bitmap (2htdp->bitmap tmp))]
           [result (bitmap w h (or description
                                   (format "the result of subtracting ~a from ~a"
                                           (image-description img2)
                                           (image-description img1))))]
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

