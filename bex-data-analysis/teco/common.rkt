#lang racket

(require db
         "../../bex/util/sql-db.rkt")

(provide (struct-out test-id)
         vec->test-id
         copy-table!
         drop-table!)

(define-struct test-id [modul index]
  #:transparent)

(define (vec->test-id row)
  (test-id (vector-ref row 0) (vector-ref row 1)))

(define (copy-table! #:src src #:dest dest)
  (drop-table! dest)
  (query-exec dbc (format "CREATE TABLE ~a AS SELECT * FROM ~a" dest src))
  dest)

(define (drop-table! table)
  (query-exec dbc (format "DROP TABLE IF EXISTS ~a" table)))
