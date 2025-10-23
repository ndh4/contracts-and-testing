#lang racket

(require db
         "../../bex/util/sql-db.rkt"
         "calculate-teco-score.rkt")

(provide (struct-out test-id)
         vec->test-id
         copy-table!
         drop-table!
         reducer/c)

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

(define reducer/c
  (->i (#:test-suite [suite string?]
                     #:test-mutant-mapping [mapping string?]
                     #:configuration [conf natural?]
                     #:get-total-order [get-total-order (-> (listof test-id?))]
                     #:choose-test [choose-test (-> (listof test-id?) test-id?)])
       [result any/c]
       #:post (suite mapping result conf)
       (= (get-mutation-score #:result-table-name mapping
                              #:test-suite-table-name result
                              #:serialized-configuration conf)
          (get-mutation-score #:result-table-name mapping
                              #:test-suite-table-name suite
                              #:serialized-configuration conf))))
