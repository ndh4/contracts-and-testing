#lang racket

(provide dbc
         tcc
         test-passed=0
         test-passed=1)

(define dbc (make-parameter #f))
(define tcc (make-parameter #f))

;; If we disable test checks, then instances of
;; failure must exclude test-check failures.
(define (test-passed=0 #:test-suite-name suite #:mapping-name mapping)
  (format
    "(   (~a.test_check_enabled = 1 AND ~a.test_passed = 0)
      OR (~a.test_check_enabled = 0 AND ~a.test_passed = 0 AND ~a.outcome != 'test-failure'))"
      suite mapping
      suite mapping mapping))

(define (test-passed=1 #:test-suite-name suite #:mapping-name mapping)
  (format "(NOT ~a)" (test-passed=0 #:test-suite-name suite #:mapping-name mapping)))