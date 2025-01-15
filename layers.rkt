#lang racket

(require csc151)

;; File:
;;   layers.rkt
;; Summary:
;;   An attempt to implement the Layers game in Racket
;; Author:
;;   Samuel A. Rebelsky

(provide (all-defined-out))

; +----------+-------------------------------------------------------
; | Settings |
; +----------+

;;; (default-size) -> positive-integer?
;;; Get the current default size

;;; (default-size size) -> integer?
;;;   size : positive-integer?
;;; Set the default size to size.
(define default-size
  (let ([ds 500])
    (lambda info
      (when (not (null? info))
        (let ([new-size (car info)])
          (if (not (positive-integer? new-size))
              (error "default-size requires a positive integer")
              (set! ds new-size))))
      ds)))
  
; +--------+---------------------------------------------------------
; | Colors |
; +--------+

; brick-red is used for four-circles
(define brick-red (rgb 164 17 17))

; bright-blue is used for missing-oval and stripes
(define bright-blue (rgb 30 145 255))

; dark-orange is used for three-rectangles
(define dark-orange (rgb 200 100 50))

; dark-purple is used for ...
(define dark-purple (rgb 75 0 130))

; light-blue 
(define light-blue (rgb 60 185 255))

; light-orange is used for both sides of four-circles
(define light-orange (rgb 255 165 0))

; light-purple
(define light-purple (rgb 95 30 150))

; medium-orange is used for one side of four-circles
(define medium-orange (rgb 240 150 0))

; red is used for both sides of two-wedges
(define red (rgb 240 0 0))

; yellow is used for missing-oval
(define yellow (rgb 225 225 0))

; yellow-green is used for missing-oval and wedges
(define yellow-green (rgb 200 220 50))

; +--------+---------------------------------------------------------
; | Pieces |
; +--------+

;;; (three-rects bgcolor fgcolor size) -> image?
;;;   bgcolor : color?
;;;   fgcolor : color?
;;;   size : positive-integer?
;;; Create a size-by-size layers piece with the given colors.
(define three-rects
  (lambda (bgcolor fgcolor size)
    (let* ([outer-edge 2]
           [inner-edge 1]
           [separator 13/8]
           [inside-space 6]
           [total (+ (* 2 outer-edge) (* 6 inner-edge) (* 3 inside-space) (* 2 separator))]
           [outer-edge-size (* size (/ outer-edge total))]
           [inner-edge-size (* size (/ inner-edge total))]
           [sep-width (* size (/ separator total))]
           [inner-width (* size (/ inside-space total))]
           [inner-height (- size (* 2 (+ inner-edge-size outer-edge-size)))]
           [inner-rectangle (outlined-rectangle inner-width inner-height fgcolor inner-edge-size)]
           [sep (solid-rectangle sep-width size bgcolor)]
           [triplet (beside inner-rectangle sep inner-rectangle sep inner-rectangle)])
      (overlay
       (beside inner-rectangle sep inner-rectangle sep inner-rectangle)
       (outlined-square (- size (* 2 outer-edge-size)) bgcolor outer-edge-size)))))

;;; (three-rects-a size) -> image?
;;;   size : positive-integer
;;; Makes the a side of the three-rects card in the given size
(define make-three-rects-a
  (cut (three-rects dark-orange dark-purple <>)))

;;; (three-rects-b size) -> image?
;;;   size : positive-integer
;;; Makes the b side of the three-rects card in the given size
(define make-three-rects-b
  (cut (three-rects dark-purple light-purple <>)))

;;; three-rects-a : image?
;;; One of the two "three rects" pieces in the base game.
(define three-rects-a
  (make-three-rects-a (default-size)))

;;; three-rects-b : image?
;;; One of the two "three rects" pieces in the base game.
(define three-rects-b
  (make-three-rects-b (default-size)))

;;; (missing-oval bgcolor fgcolor size) -> image?
;;;   bgcolor : color?
;;;   fgcolor : color?
;;;   size : positive-integer?
;;; Create a size-by-size layers piece with the given colors.
(define missing-oval
  (lambda (bgcolor fgcolor size)
    (let* ([sqrt2 (sqrt 2)]
           [background (solid-square (/ (* size 2) sqrt2) bgcolor)]
           [diameter (* 23/64 size)]
           [edge (* 1/8 size)]
           [center-thing (overlay (outlined-rectangle (- size diameter) diameter fgcolor edge)
                                  (beside (outlined-circle diameter fgcolor edge)
                                          (outlined-circle diameter fgcolor edge)))]
           [negative-part (overlay (solid-rectangle (- size diameter) diameter "white")
                                   (beside (solid-circle diameter "white")
                                           (solid-rectangle (* 2 edge) diameter "white")
                                           (solid-circle diameter "white")))]
           [temp (overlay negative-part
                          center-thing
                          background)]
           [shape (white->transparent temp)])
      (crop (rotate shape 45)
            (/ size 2)
            (/ size 2)
            size
            size))))

;;; (make-missing-oval-a size) -> image?
;;;   size : positive-integer?
;;; Create a version of the standard missing-overal piece (yellow on green).
(define make-missing-oval-a
  (cut (missing-oval yellow-green yellow <>)))

;;; (make-missing-oval-b size) -> image?
;;;   size : positive-integer?
;;; Create a version of the standard missing-overal piece (yellow on bright blue).
(define make-missing-oval-b
  (cut (missing-oval bright-blue yellow <>)))

(define missing-oval-a
  (make-missing-oval-a (default-size)))

(define missing-oval-b
  (make-missing-oval-b (default-size)))

;;; (stripes bgcolor fgcolor size) -> image?
;;;   bgcolor : color?
;;;   fgcolor : color?
;;;   size : non-negative-integer?
;;; Create a size-by-size layers piece with the given colors.
(define stripes
  (lambda (bgcolor fgcolor size)
    (let* ([sqrt2 (sqrt 2)]
           [bigger (/ (* size 2) sqrt2)]
           [line-height (* size 1/4)]
           [background (solid-square bigger bgcolor)]
           [big-shape (overlay (above (solid-rectangle bigger line-height fgcolor)
                                      (solid-rectangle bigger (* size 7/32) (rgb 0 0 0 0))
                                      (solid-rectangle bigger line-height fgcolor))
                               background)]
           [shape (crop (rotate big-shape -45)
                        (/ size 2)
                        (/ size 2)
                        size
                        size)]
           [poly (solid-polygon (list (pt 0 (* size 9/32))
                                      (pt 0 (* size 21/32))
                                      (pt (* size 21/32) 0)
                                      (pt (* size 9/32) 0))
                                "white")]
           [add-poly (lambda (img)
                       (overlay/align "left"
                                      "top"
                                      (vshift (hshift poly
                                                      (* size 3/32))
                                              (* size 3/32))
                                      img))])
      (white->transparent (add-poly (rotate (add-poly shape) 180))))))

;;; (make-stripes-a size) -> image?
;;;   size : positive-integer?
;;; Create a version of the standard stripes piece (blue on blue).
(define make-stripes-a
  (cut (stripes light-blue bright-blue <>)))

;;; (make-stripesl-b size) -> image?
;;;   size : positive-integer?
;;; Create a version of the standard missing-overal piece (yellow on bright blue).
(define make-stripes-b
  (cut (stripes dark-purple bright-blue <>)))

(define stripes-a (make-stripes-a (default-size)))
(define stripes-b (make-stripes-b (default-size)))

;;; (four-circles bgcolor fgcolor size) -> image?
;;;   bgcolor : color?
;;;   fgcolor : color?
;;;   size : non-negative-integer?
;;; Create a size-by-size layers piece with the given colors.
(define four-circles
  (lambda (bgcolor fgcolor size)
    (let* ([circ (solid-circle (* size 17/32) fgcolor)]
           [circ2 (overlay (solid-circle (* size 10/32) "white") circ)]
           [shifty (* 14/32 size)]
           [row1 (overlay/align "left" "top" circ2 (hshift circ shifty))]
           [row2 (overlay/align "left" "top" circ2 (hshift circ2 shifty))]
           [grid (overlay/align "left" "top" row1 (vshift row2 shifty))]
           [bg (solid-square size bgcolor)])
      (white->transparent (overlay grid bg)))))

;;; (make-our-circles-a size) -> image?
;;;   size : non-negative-integer?
;;; Create a size-by-size circles layers piece (orange on orange)
(define make-four-circles-a
  (cut (four-circles medium-orange light-orange <>)))

;;; (make-four-circles-b size) -> image?
;;;   size : non-negative-integer?
;;; Create a size-by-size circles layers piece (orange on red)
(define make-four-circles-b
  (cut (four-circles brick-red light-orange <>)))

(define four-circles-a (make-four-circles-a (default-size)))
(define four-circles-b (make-four-circles-b (default-size)))

;;; (make-wedge bgcolor fgcolor size) -> image?
;;;   bgcolor : color?
;;;   fgcolor : color?
;;;   size : positive-integer?
;;; Make a size-by-size layers card with two wedges.
(define two-wedges
  (lambda (bgcolor fgcolor size)
    (let* ([bg (solid-square size bgcolor)]
           [offset (* 10/32 size)]
           [radius (* 19/32 size)]
           [inner-radius (* 15/32 size)]
           [lw (* 3/32 size)]
           [full-wedge (above (solid-rectangle (+ lw radius) lw fgcolor)
                              (beside (solid-rectangle lw radius fgcolor)
                                      (rotate (solid-wedge radius 90 fgcolor) -90)))]
           [hole-wedge (overlay/align "left" "top"
                                      (vshift
                                       (hshift
                                        (rotate (solid-wedge inner-radius 90 "white") -90)
                                        lw)
                                        lw)
                                      full-wedge)]
           [two-wedges (overlay/align "left" "top"
                                      hole-wedge
                                      (hshift (vshift (rotate hole-wedge 180) offset) offset))])
      (white->transparent (overlay two-wedges bg)))))

;;; (make-two-wedges-a size) -> image?
;;;   size : positive-integer?
;;; Create a size-by-size two-wedges layers card (red on green)
(define make-two-wedges-a
  (cut (two-wedges yellow-green red <>)))

;;; (make-two-wedges-b size) -> image?
;;;   size : positive-integer?
;;; Create a size-by-size two-wedges layers card (red on red)
(define make-two-wedges-b
  (cut (two-wedges brick-red red <>)))

(define two-wedges-a (make-two-wedges-a (default-size)))
(define two-wedges-b (make-two-wedges-b (default-size)))

; +----------+-------------------------------------------------------
; | The game |
; +----------+

;;; puzzles : list-of image?
;;; All the puzzles we've made in this session
(define puzzles null)

;;; (layers count piece1a piece1b piece2a piece2b ...) -> image?
;;;   count : integer?
;;;   piece1a : image?
;;;   piece1b : image?
;;;   piece2a : image?
;;;   piece2b : image?
;;;   ...
;;; Create a layers puzzle with count layers, selecting from among the pairs of pieces
;;; (of which there must be at least count pairs).
(define layers
  (let ([standard-pieces
         (list three-rects-a three-rects-b
               missing-oval-a missing-oval-b
               stripes-a stripes-b
               four-circles-a four-circles-b
               two-wedges-a two-wedges-b)])
    (lambda (count . stuff)
      (let ([pieces (if (null? stuff) standard-pieces stuff)])
        (cond
          [(< (length pieces) (* 2 count))
           (error "Insufficiently many pieces")]
          [(< count 1)
           (error "Need to have at least one layer")]
          [else
           (let ([puzzle
                  (overlay (layers-kernel count pieces)
                           (solid-square (max (image-width (car pieces))
                                              (image-height (car pieces)))
                                         "white"))])
             (set! puzzles (cons puzzle puzzles))
             puzzle)])))))

;;; (layers-kernel count pieces) -> image?
;;;    count : integer?
;;;    pieces : list-of image?
;;; Create a layers puzzle with the given number of layers and the specified pieces.
(define layers-kernel
  (lambda (count pieces)
    (if (zero? count)
        (solid-square 0 (rgb 0 0 0 0))
        (let* ([piece-num (random (length pieces))]
               [piece (list-ref pieces piece-num)]
               [partner (if (even? piece-num) (+ piece-num 1) (- piece-num 1))]
               [num-to-take (min piece-num partner)])
          (overlay (rotate piece (list-ref (list 0 90 180 270) (random 4)))
                   (layers-kernel (- count 1) (append (take pieces num-to-take) (drop pieces (+ num-to-take 2)))))))))
