#lang racket

(require db
         "db-params.rkt"
         "calculate-teco-score.rkt")

(provide (struct-out test-id)
         vec->test-id
         (struct-out mutant-id)
         vec->mutant-id
         (struct-out slice)
         build-table-prefix
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
         maybe-make-test-suite!
         make-result-suite
         maybe-move-sanity!
         bool->sqlint)

(define-struct test-id [modul index check-enabled?]
  #:transparent)

;; Should use inheritance but I'm too lazy to look up the syntax
(define-struct mutant-id [modul index]
  #:transparent)

(define-struct slice [experiment benchmark config])

(define (vec->test-id row)
  (test-id (vector-ref row 0) (vector-ref row 1) (sqlint->bool (vector-ref row 2))))

(define (vec->mutant-id row)
  (mutant-id (vector-ref row 0) (vector-ref row 1)))

(define (build-table-prefix #:base base #:config conf #:test-check-config test-check-config)
  (format "~a_~a" base conf))

(define (copy-table! #:src src #:dest dest)
  (drop-table! dest)
  (query-exec (dbc) (format "CREATE TABLE ~a AS SELECT * FROM ~a" dest src))
  dest)

(define (drop-table! table)
  (query-exec (dbc) (format "DROP TABLE IF EXISTS ~a" table)))

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
  (define new-name (format "~a_kills" (build-table-prefix #:base mapping #:config conf #:test-check-config test-check-config)))
  (drop-table! new-name)
  (query-exec
   (dbc)
   (format
    "CREATE TABLE ~a AS
      SELECT module_under_test, test_index, test_check_enabled, mutant_module, mutation_index from ~a
      JOIN ~a USING (module_under_test, test_index) WHERE ~a AND configuration = $1"
    new-name
    mapping
    suite
    (test-passed=0 #:test-suite-name suite #:mapping-name mapping))
   conf)
  new-name)

(define (empty-table? t)
  (zero? (query-value (dbc) (format "SELECT COUNT(*) FROM ~a" t))))

(define (get-best-tests #:kills-table kills-table)
  (query-rows
   (dbc)
   (format
    "SELECT module_under_test, test_index, test_check_enabled, COUNT(*) from ~a
                                          GROUP BY module_under_test, test_index, test_check_enabled
     HAVING COUNT(*) = (SELECT(MAX(cnt)) FROM (SELECT COUNT(*) as cnt FROM ~a GROUP BY module_under_test, test_index, test_check_enabled))"
    kills-table
    kills-table)))

(define (make-result-suite #:start-suite suite #:configuration conf #:algo-name algo-name)
  (define new-name (format "~a_~a_result" (build-table-prefix #:base suite #:config conf #:test-check-config test-check-config) algo-name))
  (query-exec (dbc) (format "DROP TABLE IF EXISTS ~a" new-name))
  (query-exec (dbc) (format "CREATE TABLE ~a AS SELECT * FROM ~a WHERE FALSE" new-name suite))
  new-name)

(define (test-chosen! #:test test #:kills-table kills-table)
  (query-exec
   (dbc)
   (format
    "DELETE FROM ~a
    WHERE (mutant_module, mutation_index)
    IN (SELECT DISTINCT mutant_module, mutation_index FROM ~a WHERE module_under_test=$1 AND test_index=$2 AND test_check_enabled=$3)"
    kills-table
    kills-table)
   (test-id-modul test)
   (test-id-index test)
   (bool->sqlint (test-id-check-enabled? test))))

(define (add-test! #:test test #:suite suite)
  (query-exec (dbc)
              (format "INSERT INTO ~a VALUES ($1, $2, $3)" suite)
              (test-id-modul test)
              (test-id-index test)
              (bool->sqlint (test-id-check-enabled? test))))

(define (delete-test! #:test test #:kills-table kills-table)
  (query-exec
   (dbc)
   (format "DELETE FROM ~a
                       WHERE module_under_test=$1 AND test_index=$2 AND test_check_enabled=$3"
           kills-table)
   (test-id-modul test)
   (test-id-index test)
   (bool->sqlint (test-id-check-enabled? test))))

(define (delete-mutant! #:mutant mutant #:kills-table kills-table)
  (query-exec
   (dbc)
   (format "DELETE FROM ~a
                       WHERE mutant_module=$1 AND mutation_index=$2"
           kills-table)
   (mutant-id-modul mutant)
   (mutant-id-index mutant)))

(define (maybe-move-sanity! mapping)
  (call-with-transaction
   (dbc)
   (lambda ()
     (when (not (zero? (query-value
                        (dbc)
                        (format "SELECT COUNT(*) FROM ~a WHERE mutant_module='NO_MUTATIONS'"
                                mapping))))

       (query-exec (dbc) (format "DROP TABLE IF EXISTS ~a_sanity" mapping))

       (query-exec
        (dbc)
        (format "CREATE TABLE ~a_sanity AS SELECT * FROM ~a WHERE mutant_module='NO_MUTATIONS'"
                mapping
                mapping))

       (query-exec (dbc) (format "DELETE FROM ~a WHERE mutant_module='NO_MUTATIONS'" mapping))))))

(define (maybe-make-test-suite! #:src src-table #:dest dest-table)
  (query-exec
   (dbc)
   (format "CREATE TABLE IF NOT EXISTS ~a AS SELECT *
                                             FROM (SELECT DISTINCT module_under_test, test_index FROM ~a)
                                             JOIN (SELECT 0 as test_check_enabled WHERE ~a
                                                                          UNION ALL
                                                                          SELECT 1 WHERE ~a
                                             )"
           dest-table
           src-table
           (bool->sqlstring (or (eq? test-check-config 'both_tc)
                                (eq? test-check-config 'no_tc)))
           (bool->sqlstring (or (eq? test-check-config 'both_tc)
                                (eq? test-check-config 'yes_tc))))))

(define/contract (bool->sqlstring b)
  (-> boolean? (or/c "TRUE" "FALSE"))
  (if b "TRUE" "FALSE"))

(define/contract (sqlint->bool b)
  (-> (or/c 0 1) boolean?)
  (equal? b 1))

(define/contract (bool->sqlint b)
  (-> boolean? (or/c 0 1))
  (if b 1 0))