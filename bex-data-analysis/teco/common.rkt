#lang racket

(provide (struct-out test-id)
         vec->test-id)

(define-struct test-id [modul index]
  #:transparent)

(define (vec->test-id row)
  (test-id (vector-ref row 0) (vector-ref row 1)))
