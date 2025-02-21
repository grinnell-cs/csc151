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

;;; (colors-dir [dir]) -> string?
;;;   dir : (any-of string? false?)
;;; Get or set the current colors directory.
(define colors-dir
  (let ([dir #f])
    (lambda params
      (when (not (null? params))
        (set! dir (car params)))
      dir)))

;;; (colors-size [size]) -> positive-integer?
;;;   size : positiveinteger?
;;; Get or set the size in which colors are rendered.
(define colors-size
  (let ([csize 16])
    (lambda params
      (when (not (null? params))
        (let ([size (car params)])
          (param-check! colors-size 1 positive-integer? size)
          (set! csize size)))
      csize)))

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
              [img (2htdp:square (colors-size) "solid"
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

;;; (rgb-component? val) -> boolean?
;;;   val : any?
;;; Determines if `val` is a valid rgb component?
(define rgb-component?
  (lambda (val)
    (and (integer? val) (exact? val) (<= 0 val 255))))

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

; +---------------+--------------------------------------------------
; | Color strings |
; +---------------+

;;; (rgb-string? str) -> boolean?
;;;   str : string?
;;; Determines if str appears to be a color string.
(define rgb-string?
  (lambda (str)
    (and (rgb-string->rgb str)
         #t)))

;;; (rgb-string->rgb str) -> (any/of rgb? false?)
;;;   str : rgb-string?
;;; Convert an RGB string to an RGB color. If that's not possible,
;;; returns false.
(define rgb-string->rgb
  (lambda (str)
    (and (string? str)
         (let ([parts (string-split str "/")])
           (and (= 3 (length parts))
                (let ([components (map string->number parts)])
                  (and (andmap number? components)
                       (apply rgb components))))))))

;;; (rgb->string c) -> string?
;;;   c : rgb?
;;; Convert an RGB color to an RGB string.
(define rgb->string
  (lambda (c)
    (string-append (number->string (rgb-red c))
                   "/"
                   (number->string (rgb-green c))
                   "/"
                   (number->string (rgb-blue c)))))

;;; (color->string c) -> string?
;;;   c : color?
;;; Convert a color to an RGB string.
(define color->string
  (lambda (c)
    (rgb->string (color->rgb c))))

; +-----------------+------------------------------------------------
; | RGB hex strings |
; +-----------------+

;;; (component->hex c) -> string?
;;;   c : rgb-component?
;;; Convert an RGB component to its two-digit hex code.
(define component->hex
  (lambda (c)
    (let ([str (number->string c 16)])
      (if (= (string-length str) 1)
          (string-append "0" str)
          str))))

;;; (rgb->hex c) -> string?
;;;   c : rgb?
;;; Convert an RGB color to a hex string.
(define rgb->hex
  (lambda (c)
    (string-append (component->hex (rgb-red c))
                   (component->hex (rgb-green c))
                   (component->hex (rgb-blue c)))))

;;; (color->hex c) -> string?
;;;   c : color?
;;; Convert any color to a hex string.
(define color->hex
  (lambda (c)
    (rgb->hex (color->rgb c))))

;;; (hex->component hex) -> rgb-component?
;;;   hex : string?
;;; Convert a two-digit hex value to the corresponding RGB component.
(define hex->component
  (lambda (hex)
    (string->number hex 16)))

;;; (hex->rgb hex) -> rgb?
;;;   hex : string
;;; Convert a six-digit hex string to the corresponding RGB color.
(define hex->rgb
  (lambda (hex)
    (rgb (hex->component (substring hex 0 2))
         (hex->component (substring hex 2 4))
         (hex->component (substring hex 4 6)))))

; +------------+-----------------------------------------------------
; | HSV colors |
; +------------+

(struct hsva (hue saturation value alpha)
  #:transparent
  #:reflection-name 'hsv
  #:methods gen:custom-write
  [(define write-proc
     (lambda (val port mode)
       (write (hsv->rgb val) port)))]
  #:guard
  (lambda (hue saturation value alpha type-name)
    (param-check! hsv 1 real? hue)
    (param-check! hsv 2 real? saturation)
    (param-check! hsv 3 real? value)
    (param-check! hsv 4 real? alpha)
    (let ([h (inexact->exact (max 0 (min 360 hue)))]
          [s (inexact->exact (max 0 (min 100 saturation)))]
          [v (inexact->exact (max 0 (min 100 value)))]
          [a (inexact->exact (max 0 (min 255 alpha)))])
      (values h s v a))))

;;; (hsv hue saturation value [alpha]) -> color?
;;;   hue : (all-of nonnegative-real? (less-than 360))
;;;   saturation : (all-of nonnegative-real? (less-than 100))
;;;   value : (all-of nonnegative-real? (less-than 100))
;;;   alpha : (all-of nonnegative-real? (less-than 255))
;;; Create a new RGB color.  Values outside of the bounds
;;; are capped appropriately.
(define hsv
  (lambda (hue saturation value [alpha 255])
    (hsva hue saturation value alpha)))

;;; (hsv? val) -> boolean?
;;;   val : any?
;;; Determine if val is an HSV color.
(define hsv? hsva?)

;;; (hsv-hue c) -> integer?
;;;   c : hsv?
;;; Determine the hue of an HSV color.
(define hsv-hue hsva-hue)

;;; (hsv-saturation c) -> integer?
;;;   c : hsv?
;;; Determine the saturation of an HSV color.
(define hsv-saturation hsva-saturation)

;;; (hsv-value c) -> integer?
;;;   c : hsv?
;;; Determine the value of an HSV color.
(define hsv-value hsva-value)

;;; (hsv-alpha c) -> integer?
;;;   c : hsv?
;;; Determine the alpha of an HSV color.
(define hsv-alpha hsva-alpha)

;;; (hsv-complement c) -> c?
;;;   c : hsv?
;;; Determine the complement of an HSV color.
(define hsv-complement
  (lambda (c)
    (hsv (modulo (+ (hsv-hue c) 180) 360)
         (hsv-saturation c)
         (hsv-value c)
         (hsv-alpha c))))

;;; (rgb-hue rgb) -> Integers [0..360]
;;;   rgb : color?
;;; Get the hue from an RGB color
(define rgb-hue
  (lambda (color)
    (rgb-hue-helper (color-red color) (color-green color) (color-blue color))))

(define rgb-hue-helper
  (lambda (red green blue)
    (rgb-hue-helper2 (max red green blue) (min red green blue) red green blue)))

(define rgb-hue-helper2
  (lambda (cmax cmin red green blue)
    (cond
      [(zero? (- cmax cmin))
        (random 360)]
      [(= red cmax)
       (fix-hue (/ (- green blue) (- cmax cmin)))]
      [(= green cmax)
       (fix-hue (+ 2 (/ (- blue red) (- cmax cmin))))]
      [else
       (fix-hue (+ 4 (/ (- red green) (- cmax cmin))))])))

(define fix-hue
  (lambda (hue)
    (round (* 60 (if (< hue 0)
                     (+ hue 6)
                     hue)))))

;;; (rgb-saturation rgb) -> integer? (between 0 and 100)
;;;   rgb : color?
;;; Convert rgb to the appropriate saturation.
(define rgb-saturation
  (lambda (c)
    (rgb-saturation-helper (max (color-red c)
                                (color-green c)
                                (color-blue c))
                           (min (color-red c)
                                (color-green c)
                                (color-blue c)))))

;;; (rgb-saturation-helper cmax cmin) -> integer? [0..100]
;;;   cmax : integer? [0..255]
;;;   cmin : integer? [0..255]
;;; Find the saturation given the max and min components.
(define rgb-saturation-helper
  (lambda (cmax cmin)
    (if (= cmax 0)
        0
        (inexact->exact (round (* 100 (/ (- cmax cmin) cmax)))))))

;;; (rgb-value rgb) -> integer? [0..100]
;;;   rgb : color?
;;; Find the value.
(define rgb-value
  (lambda (rgb)
    (round (inexact->exact (* 100
                              (/ (max (rgb-red rgb)
                                      (rgb-green rgb)
                                      (rgb-blue rgb))
                                 255))))))

;;; (rgb->hsv c) -> hsv?
;;;   c : rgb?
;;; Convert an RGB color to HSV form.
(define rgb->hsv
  (lambda (c)
    (hsv (rgb-hue c) (rgb-saturation c) (rgb-value c) (rgb-alpha c))))

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

;;; (color->rgb color) -> (or/c rgb? false?)
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
       (or (rgb-string->rgb color)
           (color-name->rgb color))]
      [(hsv? color)
       (hsv->rgb color)]
      [else
       #f])))

;;; (hsv->rgb h s v) -> rgb?
;;;   color : hsv?
;;; Convert an HSV color to an RGB color.
;;; Formulae taken from https://www.rapidtables.com/convert/color/hsv-to-rgb.html
(define hsv->rgb
  (lambda (color)
    (let ([h (hsv-hue color)]
          [s (hsv-saturation color)]
          [v (hsv-value color)]
          [a (hsv-alpha color)])
      (let* ([c (* s v 1/10000)]
             [x (* c (- 1 (abs (- (mod2 (/ h 60)) 1))))]
             [m (- (/ v 100) c)]
             [cc (round (* 255 (+ c m)))]
             [cx (round (* 255 (+ x m)))]
             [c0 (round (* 255 m))])
        (cond
          [(< h 60)
           (rgb cc cx c0 a)]
          [(< h 120)
           (rgb cx cc c0 a)]
          [(< h 180)
           (rgb c0 cc cx a)]
          [(< h 240)
           (rgb c0 cx cc a)]
          [(< h 300)
           (rgb cx c0 cc a)]
          [else
           (rgb cc c0 cx a)])))))

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

; +-----------------------+------------------------------------------
; | Color transformations |
; +-----------------------+

;;; (rgb-darker c) -> rgb?
;;;   c : rgb?
;;; Compute a darker version of `c` (if possible).
(define rgb-darker
  (lambda (c)
    (rgb (- (rgb-red c) 16)
         (- (rgb-green c) 16)
         (- (rgb-blue c) 16)
         (rgb-alpha c))))

;;; (rgb-lighter c) -> rgb?
;;;   c : rgb?
;;; Compute a lighter version of `c` (if possible).
(define rgb-lighter
  (lambda (c)
    (rgb (+ (rgb-red c) 16)
         (+ (rgb-green c) 16)
         (+ (rgb-blue c) 16)
         (rgb-alpha c))))

;;; (rgb-bluer c) -> rgb?
;;;   c : rgb?
;;; Compute a bluer version of `c` (if possible).
(define rgb-bluer
  (lambda (c)
    (rgb (- (rgb-red c) 16)
         (- (rgb-green c) 16)
         (+ (rgb-blue c) 32)
         (rgb-alpha c))))

;;; (rgb-greener c) -> rgb?
;;;   c : rgb?
;;; Compute a greener version of `c` (if possible).
(define rgb-greener
  (lambda (c)
    (rgb (- (rgb-red c) 16)
         (+ (rgb-green c) 32)
         (- (rgb-blue c) 16)
         (rgb-alpha c))))

;;; (rgb-redder c) -> rgb?
;;;   c : rgb?
;;; Compute a redder version of `c` (if possible).
(define rgb-redder
  (lambda (c)
    (rgb (+ (rgb-red c) 32)
         (- (rgb-green c) 16)
         (- (rgb-blue c) 16)
         (rgb-alpha c))))

;;; (rgb-pseudo-complement c) -> rgb?
;;;   c : rgb?
;;; Compute the pseudo-complement of `c`, a color, that when added to `c`,
;;; gives you white.
(define rgb-pseudo-complement
  (lambda (c)
    (rgb (- 255 (rgb-red c))
         (- 255 (rgb-green c))
         (- 255 (rgb-blue c))
         (rgb-alpha c))))

;;; (rgb-complement c) -> rgb?
;;;   c : rgb?
;;; Compute the complement of c.
(define rgb-complement
  (lambda (c)
    (hsv->rgb (hsv-complement (rgb->hsv c)))))

;;; (rgb-greyscale c) -> rgb?
;;;   c : rgb?
;;; Convert `c` to greyscale (or approximately greyscale).
(define rgb-greyscale
  (lambda (c)
     (let ([ave (+ (* 0.30 (rgb-red c))
                   (* 0.59 (rgb-green c))
                   (* 0.11 (rgb-blue c)))])
       (rgb ave ave ave (rgb-alpha c)))))

;;; (rgb-phaseshift c) -> rgb?
;;;   c : rgb?
;;; Shift each component of `c` by 128, adding 128 to values less than
;;; 128 and subtracting 128 from values greater than or equal to 128.
(define rgb-phaseshift
  (lambda (c)
    (rgb (remainder (+ (rgb-red c) 128) 256)
         (remainder (+ (rgb-green c) 128) 256)
         (remainder (+ (rgb-blue c) 128) 256)
         (rgb-alpha c))))

;;; (rgb-rotate-components c) -> rgb?
;;;   c : rgb?
;;; "Rotate" the three components of `c`.
(define rgb-rotate-components
  (lambda (c)
    (rgb (rgb-green c)
         (rgb-blue c)
         (rgb-red c)
         (rgb-alpha c))))

;;; (rgb-thin c) -> rgb?
;;;   c : rgb?
;;; Make `c` more transparent (if possible).
(define rgb-thin
  (lambda (c)
    (rgb (rgb-red c)
         (rgb-green c)
         (rgb-blue c)
         (- (rgb-alpha c) 32))))

;;; (rgb-thicken c) -> rgb?
;;;   c : rgb?
;;; Make `c` more opaque (if possible).
(define rgb-thicken
  (lambda (c)
    (rgb (rgb-red c)
         (rgb-green c)
         (rgb-blue c)
         (+ (rgb-alpha c) 32))))

; +--------------------+---------------------------------------------
; | Color combinations |
; +--------------------+

;;; (rgb-add c1 c2) -> rgb?
;;;   c1 : rgb?
;;;   c2 : rgb?
;;; Create a new RGB color by subtracting each component of `c2` to
;;; the corresponding component of `c1`. Retain the alpha value of `c1`.
(define rgb-add
  (lambda (c1 c2)
    (param-check! rgb-add 1 rgb? c1)
    (param-check! rgb-add 2 rgb? c2)
    (rgb (+ (rgb-red c1) (rgb-red c2))
         (+ (rgb-green c1) (rgb-green c2))
         (+ (rgb-blue c1) (rgb-blue c2))
         (rgb-alpha c1))))

;;; (rgb-subtract c1 c2) -> rgb?
;;;   c1 : rgb?
;;;   c2 : rgb?
;;; Create a new RGB color by subtracting each component of `c2` from
;;; the corresponding component of `c1`.
(define rgb-subtract
  (lambda (c1 c2)
    (param-check! rgb-subtract 1 rgb? c1)
    (param-check! rgb-subtract 2 rgb? c2)
    (rgb (- (rgb-red c1) (rgb-red c2))
         (- (rgb-green c1) (rgb-green c2))
         (- (rgb-blue c1) (rgb-blue c2))
         (rgb-alpha c1))))

;;; (rgb-average c1 c2) -> rgb?
;;;   c1 : rgb?
;;;   c2 : rgb?
;;; Create a new RGB color by averaging the corresponding components of
;;; c1 and c2.
(define rgb-average
  (lambda (c1 c2)
    (param-check! rgb-average 1 rgb? c1)
    (param-check! rgb-average 2 rgb? c2)
    (rgb (* 1/2 (+ (rgb-red c1) (rgb-red c2)))
         (* 1/2 (+ (rgb-green c1) (rgb-green c2)))
         (* 1/2 (+ (rgb-blue c1) (rgb-blue c2)))
         (* 1/2 (+ (rgb-alpha c1) (rgb-alpha c2))))))

; +-----------------+------------------------------------------------
; | A few constants |
; +-----------------+

;;; transparent -> rgb?
;;; The "transparent" (invisible) color.
(define transparent (rgb 0 0 0 0))
