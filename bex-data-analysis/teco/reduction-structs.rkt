#lang racket

(provide (struct-out test-id)
         vec->test-id
         (struct-out mutant-id)
         vec->mutant-id
         (struct-out slice)
         bool->sqlint)

(define-struct test-id [modul index check-enabled?]
  #:transparent)

(define-struct mutant-id [modul index]
  #:transparent)

(define-struct slice [experiment benchmark config])

(define (vec->test-id row)
  (test-id (vector-ref row 0) (vector-ref row 1) (sqlint->bool (vector-ref row 2))))

(define (vec->mutant-id row)
  (mutant-id (vector-ref row 0) (vector-ref row 1)))

(define/contract (sqlint->bool b)
  (-> (or/c 0 1) boolean?)
  (equal? b 1))

(define/contract (bool->sqlint b)
  (-> boolean? (or/c 0 1))
  (if b 1 0))