#lang racket

(require "../type-predicates.rkt")

;;; Tests for nonnegative

(require rackunit)

(test-true "zero is not negative (exact)" (nonnegative? 0))
(test-true "zero is not negative (inexact)" (nonnegative? 0.0))
(test-true "negative zero is not negative (exact)" (nonnegative? -0))
(test-true "a small exact integer is nonnegative" (nonnegative? 3))
(test-true "a small rational is nonnegative" (nonnegative? 1/312231))
(test-true "a small real is nonnegative" (nonnegative? 0.00000001))
(test-true "a large exact integer is nonnegative" (nonnegative? (expt 3 100)))
(test-true "a large rational is nonnegative" (nonnegative? (expt 11/3 100)))
(test-true "a large real is nonnegative" (nonnegative? (expt 1.23 101)))

(test-false "-1 is not nonnegative" (nonnegative? -1))
(test-false "a small negative exact integer is not nonnegative" (nonnegative? -3))
(test-false "a small negative rational is not nonnegative" (nonnegative? -1/11))
(test-false "a small negative real is not nonnegative" (nonnegative? -0.0000002))

