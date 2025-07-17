#lang at-exp racket

(require db
         racket/runtime-path
         "../orchestration/experiment-info.rkt")

(define-runtime-path db-path "../dbs/sqlite/teco2.sqlite3")

(define dbc (sqlite3-connect #:database db-path #:mode 'create))

;(define (ensure-test-suite-table! table-name)
;  (query-exec
;   dbc
;   (format
;    "CREATE TABLE IF NOT EXISTS ~a (
;  module_under_test TEXT,
;  test_index INTEGER,
;  PRIMARY KEY (module_under_test, test_index)
;  ON CONFLICT REPLACE
;  )"
;    table-name)))

;(define (add-test-to-suite! table-name #:module_under_test module_under_test #:test_index test_index)
;  (query-exec
;   dbc
;   (format
;    "INSERT OR REPLACE INTO ~a
;                    (module_under_test, test_index)
;                    VALUES($2, $3)"
;    table-name)
;   module_under_test
;   test_index))

(define (make-test-suite! src-table dest-table)
  (query-exec dbc
              (format "CREATE TABLE ~a AS SELECT DISTINCT module_under_test, test_index from ~a"
                      dest-table
                      src-table)))

(make-test-suite! experiment-name (string-append experiment-name "_tests"))
