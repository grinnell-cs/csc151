#lang racket

;;; File:
;;;   line.rkt
;;; Summary:
;;;   A variety of procedures for working with lines.
;;; Author:
;;;   Samuel A. Rebelsky

(require racket/generic)
(require lang/posn)

(require "sstruct.rkt")
(require "cloneable.rkt")
(permit-cloneable)
(permit-done)

(require "type-predicates.rkt")
(require "point.rkt")

(provide (all-defined-out))

; +----------------------+-------------------------------------------
; | Lines (mathematical) |
; +----------------------+

; ax + b
(struct line (slope y-intercept) 
  #:transparent
  #:guard
  (lambda (slope y-intercept type-name)
    (param-check! line 1 real? slope)
    (param-check! line 2 real? y-intercept)
    (values slope y-intercept)))

;;; (line-apply line x) -> real?
;;;   line : line?
;;;   x : real?
;;; Apply a line function to an x value, thereby giving the
;;; y value.
(define line-apply
  (lambda (line x)
    (+ (* (line-slope line) x)
       (line-y-intercept line))))

;;; (line-between pt1 pt2) -> (or line? real?)
;;;   pt1 : point?
;;;   pt2 : point?
;;; Compute the function of the line between pt1 and pt2. Returns
;;; the x intercept for vertical lines
(define line-between
  (lambda (pt1 pt2)
    (let ([hoff (- (pt-x pt2) (pt-x pt1))]
          [voff (- (pt-y pt2) (pt-y pt1))])
      (if (zero? hoff)
          (pt-x pt1)
          (let* ([slope (/ voff hoff)]
                 [y-intercept (- (pt-y pt1) (* slope (pt-x pt1)))])
            (line slope y-intercept))))))

;;; (offset-line l distance updown) -> line?
;;;   l : (any-of line? real?)
;;;   distance : non-negative-real?
;;;   updown : one of 'up or 'down
;;; Offset the line up or down so that the distance between the two lines
;;; is distance.
(define offset-line
  (lambda (l distance updown)
    ; (println (list 'offset-line line distance updown))
    (let* ([mult (if (equal? updown 'up) -1 1)]
           [a (line-slope l)]
           [b (line-y-intercept l)])
      ; Note: Computation is going from ax + 0 to ax + c
      (if (zero? a)
          (line a (+ b (* mult distance)))
          (let* ([inverse (/ -1 a)]
                 [x (/ distance (sqrt (+ 1 (sqr inverse))))]
                 [c (* x (+ inverse (- a)))])
            (line a (+ b (* mult (abs c)))))))))

(define offset-line-old
  (lambda (l distance updown)
    (println (list 'offset-line line distance updown))
    (let* ([mult (if (equal? updown 'up) -1 1)]
           [slope (line-slope l)])
      (if (zero? slope)
          (line slope (+ (line-y-intercept l) (* mult distance)))
          (let* ([inverse (/ -1 slope)]
                 [hoff (sqrt (/ (sqr distance)
                                (+ 1 (sqr inverse))))]
                 [voff (abs (* inverse hoff))])
            (line slope
                    (+ (line-apply l hoff)
                       (* mult (abs voff)))))))))

;;; (intersection line1 line2) -> (any-of pt? false?)
;;;   line1 : (any-of line? real?)
;;;   line2 : (any-of line? real?)
;;; Find the point of intersection between `line1` and `line2`. Returns
;;; `#f` if they don't intersect. Lines represented as reals are vertical
;;; and the real number is the x intercept.
(define intersection
  (lambda (line1 line2)
    (cond
      [(and (real? line1) (real? line2))
       #f]
      [(real? line1)
       (pt line1 (line-apply line2 line1))]
      [(real? line2)
       (pt line2 (line-apply line1 line2))]
      [(equal? (line-slope line1) (line-slope line2))
       #f]
      [else
       (let ([x (/ (- (line-y-intercept line2) (line-y-intercept line1))
                   (- (line-slope line1) (line-slope line2)))])
         (pt x (line-apply line1 x)))])))

;;; (intersections lines) -> (list-of pt?)
;;;   lines : (list-of (any-of line? real?))
;;; Find the intersections between successive lines. If two lines are
;;; parallel, skips that intersection.
(define intersections
  (lambda (lines)
    (let kernel ([remaining (append lines (list (car lines)))])
      (if (null? (cdr remaining))
          null
          (let ([point (intersection (car remaining) (cadr remaining))])
            (if point
                (cons point (kernel (cdr remaining)))
                (kernel (cdr remaining))))))))

