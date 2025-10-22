#lang racket

(require db
         "../calculate-teco-score.rkt"
         "../../../bex/util/sql-db.rkt"
         "../common.rkt")

(provide reduce-by-vanilla-greedy)

; Implementation:
; Keep count of num mutants that each test kills?
; OR generate that count dynamically every time.
; U should be a table of test-ids.
; R should be a table of test-ids as well.
; We need a way to remove mutants from M_t sets.
; - Keep one table of mutants per test
; - How to name it then?
; OR keep a table of the kills relation.
; -- only have kills=#t things in that table

(define (reduce-by-vanilla-greedy #:test-suite suite
                                  #:test-mutant-mapping mapping
                                  #:configuration conf
                                  #:get-total-order get-total-order
                                  #:choose-test choose-test)

  (define kills-table
    (make-kills-table #:test-suite suite #:test-mutant-mapping mapping #:configuration conf))

  (define result-suite (make-result-suite #:start-suite suite #:configuration conf))

  (let loop ()
    (cond
      [(empty-table? kills-table)
       (drop-table! kills-table)
       result-suite]
      [else
       ; let best-tests be the set of tests with max aura
       (define best-tests-vec (get-best-tests #:kills-table kills-table))
       (define best-tests (map vec->test-id best-tests-vec))

       ; call choose-test on best-tests to get t
       (define best-test (choose-test best-tests))

       ; add t to result suite
       (add-test! #:test best-test #:suite result-suite)

       ; remove every mutant that t kills from kills-table
       (remove-mutants! #:test best-test #:kills-table kills-table)
       (loop)])))

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

(define (make-result-suite #:start-suite suite #:configuration conf)
  (define new-name (format "~a_~a_greedy_result" suite conf))
  (query-exec dbc (format "DROP TABLE IF EXISTS ~a" new-name))
  (query-exec dbc (format "CREATE TABLE ~a AS SELECT * FROM ~a WHERE FALSE" new-name suite))
  new-name)

(define (remove-mutants! #:test test #:kills-table kills-table)
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
