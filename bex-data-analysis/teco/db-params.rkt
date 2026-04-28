#lang racket

(provide dbc
         test-passed=0
         test-passed=1
         dtc?-name)

(define dbc (make-parameter #f))

(define disable-test-checks? #f)

;; If we disable test checks, then instances of
;; failure must exclude test-check failures.
(define test-passed=0
  (if disable-test-checks?
    "(test_passed = 0 AND outcome!='test-failure')"
    "(test_passed = 0)"))

;; If we disable test checks, then instances of
;; passage must include test-check failures.
(define test-passed=1
  (if disable-test-checks?
    "(test_passed = 1 OR outcome='test-failure')"
    "(test_passed = 1)"))

(define dtc?-name
  (if disable-test-checks?
      "no_tc"
      "yes_tc"))