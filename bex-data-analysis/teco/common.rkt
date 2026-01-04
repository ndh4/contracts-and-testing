#lang racket

(require db
         "../../bex/util/sql-db.rkt"
         "calculate-teco-score.rkt")

(provide (struct-out test-id)
         vec->test-id
         (struct-out mutant-id)
         vec->mutant-id
         copy-table!
         drop-table!
         reducer/c
         make-kills-table
         empty-table?
         get-best-tests
         test-chosen!
         add-test!
         delete-test!
         delete-mutant!
         make-result-suite)

(define-struct test-id [modul index]
  #:transparent)

;; Should use inheritance but I'm too lazy to look up the syntax
(define-struct mutant-id [modul index]
  #:transparent)

(define (vec->test-id row)
  (test-id (vector-ref row 0) (vector-ref row 1)))

(define (vec->mutant-id row)
  (mutant-id (vector-ref row 0) (vector-ref row 1)))

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

(define (make-kills-table #:test-suite suite #:test-mutant-mapping mapping #:configuration conf)
  ; SQL SELECT [mutant-id] [test-id] [conf] WHERE test_passed = 0
  (define new-name (format "~a_~a_kills" mapping conf))
  (drop-table! new-name)
  (query-exec
   dbc
   (format
    "CREATE TABLE ~a AS
      SELECT module_under_test, test_index, mutant_module, mutation_index from ~a
      WHERE (module_under_test, test_index) IN ~a AND test_passed = 0 AND configuration = $1"
    new-name
    mapping
    suite)
   conf)
  new-name)

(define (empty-table? t)
  (zero? (query-value dbc (format "SELECT COUNT(*) FROM ~a" t))))

(define (get-best-tests #:kills-table kills-table)
  (query-rows
   dbc
   (format
    "SELECT module_under_test, test_index, COUNT(*) from ~a
                                          GROUP BY module_under_test, test_index
     HAVING COUNT(*) = (SELECT(MAX(cnt)) FROM (SELECT COUNT(*) as cnt FROM ~a GROUP BY module_under_test, test_index))"
    kills-table
    kills-table)))

(define (make-result-suite #:start-suite suite #:configuration conf #:algo-name algo-name)
  (define new-name (format "~a_~a_~a_result" suite conf algo-name))
  (query-exec dbc (format "DROP TABLE IF EXISTS ~a" new-name))
  (query-exec dbc (format "CREATE TABLE ~a AS SELECT * FROM ~a WHERE FALSE" new-name suite))
  new-name)

(define (test-chosen! #:test test #:kills-table kills-table)
  (query-exec
   dbc
   (format
    "DELETE FROM ~a
    WHERE (mutant_module, mutation_index)
    IN (SELECT DISTINCT mutant_module, mutation_index FROM ~a WHERE module_under_test=$1 AND test_index=$2)"
    kills-table
    kills-table)
   (test-id-modul test)
   (test-id-index test)))

(define (add-test! #:test test #:suite suite)
  (query-exec dbc
              (format "INSERT INTO ~a VALUES ($1, $2)" suite)
              (test-id-modul test)
              (test-id-index test)))

(define (delete-test! #:test test #:kills-table kills-table)
  (query-exec
   dbc
   (format "DELETE FROM ~a
                       WHERE module_under_test=$1 AND test_index=$2"
           kills-table)
   (test-id-modul test)
   (test-id-index test)))

(define (delete-mutant! #:mutant mutant #:kills-table kills-table)
  (query-exec
   dbc
   (format "DELETE FROM ~a
                       WHERE mutant_module=$1 AND mutation_index=$2"
           kills-table)
   (mutant-id-modul mutant)
   (mutant-id-index mutant)))
