#lang racket

(provide dbc
         tcc
         test-failed?
         test-passed?)

(define dbc (make-parameter #f))
(define tcc (make-parameter #f))

;; If we disable test checks, then instances of
;; failure must exclude test-check failures.
(define (test-failed? #:test-suite-name suite #:mapping-name mapping)
  (format "(NOT ~a)" (test-passed? #:test-suite-name suite #:mapping-name mapping)))

(define (test-passed? #:test-suite-name suite #:mapping-name mapping)
  (format
    "((~a.outcome = 'completed' OR ~a.outcome = 'skipped')
      OR ((~a.test_check_enabled = 0) AND (~a.outcome = 'test-failure'))
      OR ((~a.test_type = 'rand') AND (~a.outcome = 'timeout'))
      OR ((~a.test_type = 'rand') AND (~a.outcome = 'oom')))"
      mapping mapping
      suite mapping
      suite mapping
      suite mapping))