#lang racket

;;; File:
;;;   image-text.rkt
;;; Summary:
;;;   A variety of procedures for working with text as an image.
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
; | Fonts |
; +-------+

(sstruct %font (face family style weight underline?)
  #:reflection-name 'font
  #:transparent)

(define font? %font?)
(define font-face %font-face)
(define font-family %font-family)
(define font-style %font-style)
(define font-weight %font-weight)
(define font-underline? %font-underline?)

;;; (font face family style weight underline?)
;;;   face : (any-of string? false?)
;;;   family : (one-of "default" "decorative" "roman" "script"
;;;                    "swiss" "modern" "symbol" "system")
;;;   style :  (one-of "normal" "italic")
;;;   weight : (one-of "normal" "bold" "light")
;;;   underline? : boolean?
;;; Create a font value for use in building text.
(define font
  (lambda (face family style weight underline?)
    (param-check! font 1 (any-of string? false?) face)
    (param-check! font 2 
                  (one-of "default" "decorative" "roman" "script"
                          "swiss" "modern" "symbol" "system") 
                  family)
    (param-check! font 3 (one-of "normal" "italic") style)
    (param-check! font 4 (one-of "normal" "bold" "light") weight)
    (param-check! font 5 boolean? underline?)
    (%font face family style weight underline?)))

;;; (default-font [default]) -> font
;;;   default : font?
;;; Get or set the defalt font.
(define default-font
  (let ([deffont (font #f "default" "normal" "normal" #f)])
    (lambda params
      (when (not (null? params))
        (let ([default (car params)])
          (param-check! default-font 1 font? default)
          (set! deffont default)))
      deffont)))

; +-------------+----------------------------------------------------
; | Text basics |
; +-------------+

(sstruct %text %shape (string size font)
  #:cloneable
  #:methods gen:img-make-desc
  [(define image-make-desc
     (lambda (img)
       (format "the text [~a]"
               (text-string img))))]
  #:methods gen:img-make-pict
  [(define image-make-pict
     (lambda (img)
       (let ([font (text-font img)])
         (2htdp:text/font (text-string img)
                          (text-size img)
                          (color->2htdp (image-color img))
                          (font-face font)
                          (font-family font)
                          (font-style font)
                          (font-weight font)
                          (font-underline? font)))))]
  #:methods gen:img-make-stru
  [(define image-make-stru
     (lambda (img)
       (list 'text 
             (text-string img) 
             (text-size img) 
             (image-color img)
             (text-font img))))]
  #:done)

;;; (text? img) -> boolean?
;;;   image : image?
;;; Determines whether `img` is a text image.
(define text? %text?)

;;; (text-string text) -> string?
;;;   test : text?
;;; Grab the string associated with a text image.
(define text-string %text-string)

;;; (text-size text) -> nonnegative-exact-integer?
;;;   test : text?
;;; Grab the size of the text in a text image (in pixels).
(define text-size %text-size)

;;; (text-color text) -> rgb?
;;;   test : text?
;;; Grab the string associated with a text image.
(define text-color %basic-image-color)

;;; (text-font text) -> font?
;;;   font : text?
;;; Grab the font associated with a text image.
(define text-font %text-font)

;;; (text string size color [font]) -> text?
;;;   string : string?
;;;   size : (all-of exact-positive-integer? (less-than 256))
;;;   color : color?
;;;   font : font?
;;; Create an image of text of the given size and color, in either
;;; the specified font (if given) or the default font (if not given).
(define text
  (lambda (string size color [font (default-font)])
    (%text #f #f #f #f 
           color 
           string 
           (min 255 (max 1 size ))
           font)))

; +----------------------------+-------------------------------------
; | Additional text procedures |
; +----------------------------+

;; (find-text str img) -> (or text? #f)
;;;   img : image?
;;; Finds a text block that contains the given string, if there is
;;; one.
(define find-text
  (lambda (str img)
    (or (and (text? img)
             (string-ci=? str (text-string img))
             img)
        (and (transformed? img)
             (find-text str (subimage img)))
        (and (compound? img)
             (ormap (lambda (i) (find-text str i)) (subimages img))))))

