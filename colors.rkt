#lang racket

;;; File:
;;;   colors.rkt
;;; Author:
;;;   Samuel A. Rebelsky
;;; Summary:
;;;   Some simple utilities for dealing with colors.

(require (prefix-in 2htdp: 2htdp/image))
(require (except-in racket/draw make-color make-pen))
(require "type-predicates.rkt")

(provide (all-defined-out))

; +----------+-------------------------------------------------------
; | Settings |
; +----------+

;;; (colors-dir [dir]) -> string
;;;   dir : (any-of string? false?)
;;; Get or set the current colors directory
(define colors-dir
  (let ([dir #f])
    (lambda params
      (when (not (null? params))
        (set! dir (car params)))
      dir)))

; +---------------+--------------------------------------------------
; | RGB(A) colors |
; +---------------+

;;; (rgba-component? val) -> boolean?
;;;   val : real?
;;; Determines if val is an RGBA component (an exact integer between
;;; 0 and 255, inclusive).
(define rgba-component?
  (lambda (val)
    (and (integer? val)
         (exact? val)
         (<= 0 val 255))))

;;; (->rgba-component val) -> (or/c rgba-component? false?)
;;;   val : any/c
;;; Convert anything to an RGBA component (an exact integer between
;;; 0 and 255, inclusive). Returns false (`#f`) if it can't be converted.
(define ->rgba-component
  (lambda (val)
    (cond
      [(not (real? val))
       #f]
      [else
       (max 0 (min 255 (inexact->exact (round val))))])))

;;; (rgba r g b a) -> rgba?
;;;   r : rgba-component? 
;;;   g : rgba-component?
;;;   b : rgba-component?
;;;   a : rgba-component?
;;; Create an RGBA color.
;;;
;;; Placeholder documentation for the struct.
(struct rgba (red green blue alpha) 
  #:transparent
  #:reflection-name 'rgb
  #:methods gen:custom-write
  [(define write-proc
     (lambda (val port mode)
       (let* ([red (rgb-red val)]
              [green (rgb-green val)]
              [blue (rgb-blue val)]
              [alpha (rgb-alpha val)]
              [img (2htdp:square 16 "solid"
                                 (2htdp:color red green blue alpha))])
         (when (colors-dir)
           (when (not (equal? (format "~a" port) "#<output-port:null>"))
             (let ([dir (colors-dir)])
               (when (not (directory-exists? dir))
                 (make-directory dir))
               (let* ([zeros (lambda (num)
                               (let ([str (number->string num)])
                                 (string-append 
                                  (make-string (- 3 (string-length str)) #\0)
                                  str)))]
                      [fname (format "~a/rgb-~a-~a-~a-~a.png"
                                     dir
                                     (zeros red)
                                     (zeros green)
                                     (zeros blue)
                                     (zeros alpha))])
                 (2htdp:save-image img fname)
                 (displayln (format "![a swatch of ~a](~a)"
                                    (describe-color val) 
                                    fname))))))
         (write img port))))]
  #:guard
  (lambda (red green blue alpha type-name)
    (let ([r (->rgba-component red)]
          [g (->rgba-component green)]
          [b (->rgba-component blue)]
          [a (->rgba-component alpha)])
      (cond
        [(not r)
         (error 'rgba-component "Invalid red component ~e" red)]
        [(not g)
         (error 'rgba-component "Invalid green component ~e" green)]
        [(not b)
         (error 'rgba-component "Invalid blue component ~e" blue)]
        [(not a)
         (error 'rgba-component "Invalid alpha component ~e" alpha)]
        [else
         (values r g b a)]))))

;;; (rgb r g b) -> color?
;;;   r : real?
;;;   g : real?
;;;   b : real?
;;;   a : real? (optional)
;;; Create a new RGB color.  Values outside of the 0 ... 255 bounds 
;;; are capped appropriately.
(define rgb
  (lambda (r g b [a 255])
    (rgba r g b a)))

;;; (rgb? val) -> boolean?
;;;   val : any/c
;;; Determine if `val` is an RGB color.
(define rgb? rgba?)

;;; (rgb-red color) -> (and/c integer? (cut (<= 0 <> 255)))
;;;   color : rgba?
;;; Extract the red component of `color`.
(define rgb-red rgba-red)

;;; (rgb-green color) -> (and/c integer? (cut (<= 0 <> 255)))
;;;   color : rgba?
;;; Extract the green component of `color`.
(define rgb-green rgba-green)

;;; (rgb-blue color) -> (and/c integer? (cut (<= 0 <> 255)))
;;;   color : rgba?
;;; Extract the blue component of `color`.
(define rgb-blue rgba-blue)

;;; (rgb-alpha color) -> (and/c integer? (cut (<= 0 <> 255)))
;;;   color : rgba?
;;; Extract the alpha component of `color`.
(define rgb-alpha rgba-alpha)

;;; (rgb-distance color1 color2) -> integer?
;;;   color1 : rgb?
;;;   color2 : rgb?
;;; Find the "distance" between color1 and color2.
;;;
;;; I might want to consider rewriting this to use HSV.
(define rgb-distance
  (lambda (color1 color2)
    (+ (sqr (- (rgb-red color1) (rgb-red color2)))
       (sqr (- (rgb-green color1) (rgb-green color2)))
       (sqr (- (rgb-blue color1) (rgb-blue color2)))
       (sqr (- (rgb-alpha color1) (rgb-alpha color2))))))

; +-------------+----------------------------------------------------
; | Color names |
; +-------------+

;;; (color-name? str) -> boolean?
;;;   str : string?
;;; Determine if str is a color name
(define color-name?
  (lambda (str)
    (if (symbol? str)
        (color-name? (symbol->string str))
        (and (string? str)
             (send the-color-database find-color str)
             #t))))
             
;;; (all-color-names) -> list-of string?
;;; Get a list of all the color names.
(define all-color-names
  (lambda ()
    (send the-color-database get-names)))

;;; (find-colors name) -> list-of string?
;;;   name : string?
;;; Extract all the colors that include "name".
(define find-colors
  (lambda (name)
    (let kernel ([remaining (all-color-names)] [so-far null])
      (cond
        [(null? remaining)
         (reverse so-far)]
        [(regexp-match? name (car remaining))
         (kernel (cdr remaining) (cons (car remaining) so-far))]
        [else
         (kernel (cdr remaining) so-far)]))))

; +------------+-----------------------------------------------------
; | HSV colors |
; +------------+

; These are not yet implemented.

;;; (hsv? val) -> boolean?
;;;   val : any?
;;; Determine if `val` is an HSV color.
(define hsv?
  (lambda (val)
    false))

; +------------------+-----------------------------------------------
; | Other predicates |
; +------------------+

;;; (color? val) -> boolean?
;;;   val : any?
;;; Determines if `val` can be treated as a color.
(define color?
  (lambda (val)
    (or (rgb? val)
        (color-name? val)
        (hsv? val)
        ((is-a?/c color%) val)
        (2htdp:color? val))))

; +------------------+-----------------------------------------------
; | Color conversion |
; +------------------+

;;; (color-name->rgb name) -> (or/c color? false?)
;;;    name : (or/c symbol? string?)
;;; Convert a color name to a color. Returns false if the name cannot
;;; be converted.
(define color-name->rgb
  (lambda (name)
    (if (symbol? name)
        (color-name->rgb (symbol->string name))
        (let ([tmp (send the-color-database find-color name)])
          (color->rgb tmp)))))

;;; (rgb->color-name color) -> color-name?
;;;   color : rgb?
;;; Convert color to the name of the nearest named color.
(define rgb->color-name
  (lambda (color)
    (let kernel ([name "white"]
                 [distance (rgb-distance (rgb 255 255 255 255) color)]
                 [remaining (all-color-names)])
      (if (null? remaining)
          name
          (let* ([candidate (car remaining)]
                 [candidate-distance (rgb-distance (color-name->rgb candidate)
                                                   color)])
            (if (< candidate-distance distance)
                (kernel candidate candidate-distance (cdr remaining))
                (kernel name distance (cdr remaining))))))))

;;; (2htdp->rgb color) -> rgb?
;;;    color : 2htdp:color
;;; Convert a color from the image/2htdp library to an RGB color.
(define 2htdp->rgb
  (lambda (color)
    (rgb (2htdp:color-red color)
         (2htdp:color-green color)
         (2htdp:color-blue color)
         (2htdp:color-alpha color))))

;;; (color->rgb color) -> (or/c color? false?)
;;;   color : color? (one of the many forms)
;;; Convert one of the many forms of colors to an RGB color
(define color->rgb
  (lambda (color)
    (cond
      [(false? color)
       #f]
      [((is-a?/c color%) color)
       (rgba (send color red) (send color green) (send color blue) 
             (inexact->exact (round (* 255 (send color alpha)))))]
      [(rgb? color)
       color]
      [(2htdp:color? color)
       color]
      [(symbol? color)
       (color-name->rgb color)]
      [(string? color)
       (color-name->rgb color)]
      [(hsv? color)
       (hsv->rgb color)]
      [else
       #f])))

;;; (hsv->rgb h s v) -> color?
;;;   h : integer? [0..360]
;;;   s : integer? [0..100]
;;;   v : integer? [0..100] 
;;; Convert an HSV color to an RGB color.
;;; Formulae taken from https://www.rapidtables.com/convert/color/hsv-to-rgb.html
(define hsv->rgb
  (lambda (h s v)
    (let* ([c (* s v 1/10000)]
           [x (* c (- 1 (abs (- (mod2 (/ h 60)) 1))))]
           [m (- (/ v 100) c)]
           [cc (round (* 255 (+ c m)))]
           [cx (round (* 255 (+ x m)))]
           [c0 (round (* 255 m))])
      (cond
        [(< h 60)
         (rgb cc cx c0)]
        [(< h 120)
         (rgb cx cc c0)]
        [(< h 180)
         (rgb c0 cc cx)]
        [(< h 240)
         (rgb c0 cx cc)]
        [(< h 300)
         (rgb cx c0 cc)]
        [else
         (rgb cc c0 cx)]))))

;;; (rgb->2htdp rgb) -> 2htdp:color?
;;;   rgb : rgb?
;;; Convert an RGB color to a 2htdp color.
(define rgb->2htdp
  (lambda (rgb)
    (2htdp:color (rgb-red rgb) (rgb-green rgb) (rgb-blue rgb) (rgb-alpha rgb))))

;;; (color->2htdp color) -> 2htdp:color?
;;;   color : color?
;;; Convert a color to a 2htdp color.
(define color->2htdp
  (lambda (color)
    (if (2htdp:color? color)
        color
        (rgb->2htdp (color->rgb color)))))

;;; (color->color-name color) -> color-name?
;;;   color : color?
;;; Convert a color to a color name
(define color->color-name
  (lambda (color)
    (if (color-name? color)
        color
        (rgb->color-name (color->rgb color)))))

; +------------------+-----------------------------------------------
; | Color components |
; +------------------+

;;; (color-red color) -> (any-of integer? false?)
;;;   color : color?
;;; Gets the red component of `color`. Returns `#f` if `color` is not 
;;; one of the standard color types.
(define color-red
  (lambda (color)
    (cond
      [(rgb? color)
       (rgb-red color)]
      [((is-a?/c color%) color)
       (send color red)]
      [(2htdp:color? color)
       (2htdp:color-red color)]
      [(symbol? color)
       (color-red (send the-color-database find-color (symbol->string color)))]
      [(string? color)
       (color-red (send the-color-database find-color color))]
      [(hsv? color)
       (color-red (hsv->rgb color))]
      [else
       #f])))

;;; (red-component color) -> (any-of integer? false?)
;;;   color : color?
;;; Gets the red component of `color`. Returns `#f` if `color` is not 
;;; one of the standard color types.
;;;
;;; DEPRECATED. Replaced by `color-red`.
(define red-component color-red)

;;; (color-green color) -> (any-of integer? false?)
;;;   color : color?
;;; Gets the green component of `color`. Returns `#f` if `color` is not 
;;; one of the standard color types.
(define color-green
  (lambda (color)
    (cond
      [(rgb? color)
       (rgb-green color)]
      [((is-a?/c color%) color)
       (send color green)]
      [(2htdp:color? color)
       (2htdp:color-green color)]
      [(symbol? color)
       (color-green (send the-color-database find-color (symbol->string color)))]
      [(string? color)
       (color-green (send the-color-database find-color color))]
      [(hsv? color)
       (color-green (hsv->rgb color))]
      [else
       #f])))

;;; (green-component color) -> (any-of integer? false?)
;;;   color : color?
;;; Gets the green component of `color`. Returns `#f` if `color` is not 
;;; one of the standard color types.
;;;
;;; DEPRECATED. Replaced by `color-green`.
(define green-component color-green)

;;; (color-blue color) -> (any-of integer? false?)
;;;   color : color?
;;; Gets the blue component of `color`. Returns `#f` if `color` is not 
;;; one of the standard color types.
(define color-blue
  (lambda (color)
    (cond
      [(rgb? color)
       (rgb-blue color)]
      [((is-a?/c color%) color)
       (send color blue)]
      [(2htdp:color? color)
       (2htdp:color-blue color)]
      [(symbol? color)
       (color-blue (send the-color-database find-color (symbol->string color)))]
      [(string? color)
       (color-blue (send the-color-database find-color color))]
      [(hsv? color)
       (color-blue (hsv->rgb color))]
      [else
       #f])))

;;; (blue-component color) -> (any-of integer? false?)
;;;   color : color?
;;; Gets the blue component of `color`. Returns `#f` if `color` is not 
;;; one of the standard color types.
;;;
;;; DEPRECATED. Replaced by `color-blue`.
(define blue-component color-blue)

;;; (color-alpha color) -> (any-of integer? false?)
;;;   color : color?
;;; Gets the alpha component of `color`. Returns `#f` if `color` is not 
;;; one of the standard color types.
(define color-alpha
  (lambda (color)
    (cond
      [(rgb? color)
       (rgb-alpha color)]
      [((is-a?/c color%) color)
       (inexact->exact (round (* 255 (send color alpha))))]
      [(2htdp:color? color)
       (2htdp:color-alpha color)]
      [(symbol? color)
       (color-alpha (send the-color-database find-color (symbol->string color)))]
      [(string? color)
       (color-alpha (send the-color-database find-color (string? color)))]
      [(hsv? color)
       (color-alpha (hsv->rgb color))]
      [else
       #f])))

;;; (alpha-component color) -> (any-of integer? false?)
;;;   color : color?
;;; Gets the alpha component of `color`. Returns `#f` if `color` is not 
;;; one of the standard color types.
;;;
;;; DEPRECATED. Replaced by `color-alpha`.
(define alpha-component color-alpha)

; +--------------------------+---------------------------------------
; | Miscellaneous Procedures |
; +--------------------------+

;;; (mod2 x) -> real?
;;;   x : real?
;;; Like modulo base 2, but for real numbers.
;;;
;;; Used primarily for hsv->rgb.
(define mod2
  (lambda (x)
    (cond 
      [(< x 0)
       (- 2 (mod2 (- x)))]
      [(< x 2)
       x]
      [else
       (mod2 (- x 2))])))

;;; (color-equal? c1 c2) -> boolean?
;;;   c1 : color?
;;;   c2 : color?
;;; Determine if `c1` and `c2` represent the same color.
(define color-equal?
  (lambda (c1 c2)
    (equal? (color->rgb c1) (color->rgb c2))))

;;; (describe-color c) -> string?
;;;   c : color?
;;; Create an appropriate description of c.
(define describe-color
  (lambda (c)
    (let* ([crgb (color->rgb c)]
           [name (color->color-name c)]
           [alpha (rgb-alpha crgb)]
           [nrgb (color->rgb name)]
           [prefix (if (and (= (rgb-red crgb) (rgb-red nrgb))
                            (= (rgb-green crgb) (rgb-green nrgb))
                            (= (rgb-blue crgb) (rgb-blue nrgb)))
                       ""
                       "approximately ")])
      (cond
        [(zero? alpha)
         "transparent"]
        [(<= alpha 64)
         (string-append "mostly-transparent " prefix name)]
        [(<= alpha 128)
         (string-append "semi-transparent " prefix name)]
        [(<= alpha 192)
         (string-append "semi-opaque " prefix name)]
        [(<= alpha 254)
         (string-append "mostly-opaque " prefix name)]
        [else
         (string-append prefix name)]))))

;;; (color->list c) -> (list-of integer?)
;;;   c : color?
;;; Create a list containing the four rgb components of c.
(define color->list
  (lambda (c)
    (let ([crgb (color->rgb c)])
      (list (rgb-red crgb)
            (rgb-green crgb)
            (rgb-blue crgb)
            (rgb-alpha crgb)))))
