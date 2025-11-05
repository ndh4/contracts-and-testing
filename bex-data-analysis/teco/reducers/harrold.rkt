#lang racket

(require db
         "../calculate-teco-score.rkt"
         "../../../bex/util/sql-db.rkt"
         "../common.rkt")

(provide reduce-by-harrold)

(define (reduce-by-harrold #:test-suite suite
                           #:test-mutant-mapping mapping
                           #:configuration conf
                           #:get-total-order get-total-order
                           #:choose-test choose-test)

  ;; "kills" binary relation
  (define kills-table
    (make-kills-table #:test-suite suite
                      #:test-mutant-mapping mapping
                      #:configuration conf))

  ;; mapping from mutants to cardinality / killed?
  (define mutant-card-table
    (make-mutant-card-table #:kills-table kills-table))

  ;; maximum cardinality of any test in the list
  (define max-card (max-card #:mutant-card-table mutant-card-table))

  ;; begin with all tests that kill mutants of cardinality 1 and add them to the
  ;; table of results
  (define tests-for-card-1
    (tests-for-card 1
                    #:kills-table kills-table
                    #:mutant-card-table mutant-card-table))

  (define result-name (format "~a_~a_linsearch_result" suite conf))
  #;(define result-table (copy-table! #:src tests-for-card-1 #:dst result-name))
  ;; TODO: add to result-table
  ;; TODO: kill mutants
  ;; probably don't want to copy but rather create a add-tests-to-result
  ;; function or something like that?
  ;; or maybe just accumulate in a list and then join
  ;; or even just keep all data in testlist to begin with

  ;; TODO use named let or do loop instead
  (for ([cur-card (in-range 2 max-card)])
    #:break (= cur-card (max-card #:mutant-card-table mutant-card-table))
    (define (add-tests-until-covered)
      (define tests-for-cur-card
        (tests-for-card cur-card
                        #:kills-table kills-table
                        #:mutant-card-table mutant-card-table))
      (unless (tbl-empty? tests-for-cur-card)
        ;; TODO: add to result-table
        ;; TODO: kill mutants
        (add-tests-until-covered))
      )
    )

  ;; "result" should have the same columns as "suite"
  result-name)

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
    ;; won't (module_under_test, test_index) always be in the test suite, since
    ;; the test suite is defined as the unique module/test pairs of the mapping?
    new-name
    mapping
    suite)
   conf)
  new-name)

(define (make-mutant-card-table #:kills-table kills)
  (define new-name (format "~a_card" kills))
  (drop-table! new-name)
  (query-exec
   dbc
   (format
    "CREATE TABLE ~a AS
      SELECT mutant_module, mutation_index, COUNT(*) AS card, FALSE AS killed FROM ~a
      GROUP BY mutant_module, mutation_index"
    new-name
    kills))
  new-name)

(define (max-card #:mutant-card-table mutant-card)
  (query-value
   dbc
   (format
    "SELECT MAX(card) FROM ~a"
    mutant-card)))

;; get the set of tests that kill unkilled mutants of cardinality CARD
;; verified to work, i believe
;; TODO should LIST be a Racket list or SQL table?
(define (tests-for-card card #:kills-table kills #:mutant-card-table mutant-card)
  (define new-name "LIST")
  (drop-table! new-name)
  (query-exec
   dbc
   (format
    "CREATE TABLE ~a AS
      SELECT DISTINCT module_under_test, test_index FROM ~a Kills
      INNER JOIN ~a MutantCard
       ON (Kills.mutant_module=MutantCard.mutant_module AND
           Kills.mutation_index=MutantCard.mutation_index)
      WHERE MutantCard.card = $1"
    new-name
    kills
    mutant-card)
   card)
  new-name)

;; get the set of tests from TESTLIST that kill the most unkilled mutants of
;; cardinality CARD
;; not tested and probably wrong
(define (best-tests-from-list-for-card card #:lst-table lst #:mutant-card-table mutant-card)
  (define new-name (format "COUNT_~a" card))
  (drop-table! new-name)
  (query-exec
   (format
    "CREATE TABLE ~a AS
      SELECT module_under_test, test_index FROM ~a List
      INNER JOIN (SELECT * FROM ~a WHERE card = $1 AND killed = FALSE) MutantCard
       ON (List.module_under_test=MutantCard.module_under_test AND
           List.test_index=MutantCard.test_index)
      GROUP BY MutantCard.module_under_test, MutantCard.test_index
      WHERE COUNT(*) = MAX(COUNT(*))"
    lst
    mutant-card)
   card))

;; TODO don't actually create a new table for each recursive call; just shave
;; down the one that already exists
(define (select-test card #:test-list lst)
  (drop-table!))

;; it's good that LIST/TESTLIST is a sql table and not a racket list
