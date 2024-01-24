#lang racket

(require "../type-predicates.rkt")

;;; Tests for any-of

(require rackunit)

(define none?
  (any-of))

(test-false "none? returns false on a string" (none? "a"))
(test-false "none? returns false on an integer" (none? 1))
