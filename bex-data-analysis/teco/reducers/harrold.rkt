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
  
  ;; table of results
  (define result-suite (make-result-suite))
  
  ;; begin with all tests that kill mutants of cardinality 1, kill their
  ;; corresponding mutants, and add the tests to the result suite
  (define essential-tests
    (remaining-tests-for-card 1
                              #:kills-table kills-table
                              #:mutant-card-table mutant-card-table))
  (kill-mutants! #:mutant-card-table mutant-card-table
                 #:test-list essential-tests)
  (add-tests! #:to result-suite #:from essential-tests)
  
  (let card-loop ([cur-card 2])
    (define max-card (get-max-card #:mutant-card-table mutant-card-table))
    (when (cur-card . < . max-card)
      ;; add tests until all tests of this cardinality are killed
      (let add-tests-loop ()
        ;; list of tests killing unkilled mutants of cardinality CARD
        (define remaining-tests-for-cur-card
          (remaining-tests-for-card cur-card
                                    #:kills-table kills-table
                                    #:mutant-card-table mutant-card-table))
        (unless (tbl-empty? remaining-tests-for-cur-card)
          ;; select the best test from the test list
          (define best-test
            (select-test cur-card
                         #:max-card max-card
                         #:choose-test choose-test
                         #:test-list remaining-tests-for-cur-card
                         #:mutant-card-table mutant-card-table))
          
          ;; mark any mutants killed by the selected test as killed
          (kill-mutants! #:mutant-card-table mutant-card-table
                         #:test-list essential-tests)
          
          ;; add the selected test to result-table
          (add-tests! #:to result-suite #:from best-test)
          
          ;; continue adding tests
          (add-tests-loop)))

      ;; continue with the next cardinality
      (card-loop (+ cur-card 1)))))

(define (make-result-suite #:start-suite suite #:configuration conf)
  (define new-name (format "~a_~a_harrold_result" suite conf))
  (query-exec dbc (format "DROP TABLE IF EXISTS ~a" new-name))
  (query-exec dbc (format "CREATE TABLE ~a AS SELECT * FROM ~a WHERE FALSE" new-name suite))
  new-name)

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

;; Query the maximum cardinality of any unkilled mutant from MUTANT-CARD
(define (get-max-card #:mutant-card-table mutant-card)
  (query-value
   dbc
   (format
    "SELECT MAX(card) FROM ~a
     WHERE killed = FALSE"
    mutant-card)))

;; query the number of rows in TBL
(define (tbl-size tbl)
  (query-value
   dbc
   (format
    "SELECT COUNT(*) FROM ~a"
    tbl)))

;; Does TBL have any rows?
(define (tbl-empty? tbl)
  (= (tbl-size tbl) 0))

;; get the set of tests that kill unkilled mutants of cardinality CARD
;; verified to work, i believe
(define (remaining-tests-for-card card #:kills-table kills #:mutant-card-table mutant-card)
  (define new-name (format "TESTLIST_~a" card))
  (drop-table! new-name)
  (query-exec
   dbc
   (format
    "CREATE TABLE ~a AS
      SELECT DISTINCT module_under_test, test_index FROM ~a Kills
      INNER JOIN ~a MutantCard
       ON (Kills.mutant_module=MutantCard.mutant_module AND
           Kills.mutation_index=MutantCard.mutation_index)
      WHERE MutantCard.card = $1 AND MutantCard.killed = false"
    new-name
    kills
    mutant-card)
   card)
  new-name)

;; get the set of tests from TESTLIST that kill the most unkilled mutants of
;; cardinality CARD
;; not tested and probably wrong
(define (best-tests-from-list-for-card card #:lst-table lst #:mutant-card-table mutant-card)
  (define new-name (format "~a_FILTER_~a" lst card))
  (drop-table! new-name)
  (query-exec
   dbc
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

;; Add tests from TEST-LIST to TO-TABLE
(define (add-tests! #:to to-table #:from test-list #:start-suite suite)
  (query-exec
   dbc
   (format
    "INSERT INTO ~a
      SELECT * FROM ~a Suite
      WHERE EXISTS (
        SELECT 1
        FROM ~a as List
        WHERE (List.module_under_test=Suite.module_under_test AND
               List.test_index=Suite.test_index)
      )"
    to-table
    suite
    test-list)))

;; Mark all mutants from TEST-LIST as killed
(define (kill-mutants! #:mutant-card-table mutant-card-table
                       #:test-list test-list)
  (query-exec
   dbc
   (format
    "UPDATE ~a AS MutantCard
      SET killed = FALSE
      WHERE EXISTS (
        SELECT 1
        FROM ~a as List
        WHERE (List.module_under_test=MutantCard.module_under_test AND
               List.test_index=MutantCard.test_index)
      )"
    mutant-card-table
    test-list)))

;; Select the best test for cardinality CARD
(define (select-test card
                     #:max-card max-card
                     #:choose-test choose-test
                     #:test-list lst
                     #:mutant-card-table mutant-card)
  ;; Get the list of best tests for CARD
  (define best-tests
    (best-tests-from-list-for-card card
                                   #:lst-table lst
                                   #:mutant-card-table mutant-card))
  ;; If the list has multiple options, select the best option for the next
  ;; cardinality. If there is no next cardinality, choose by CHOOSE-TEST
  (cond [(= (tbl-size best-tests) 1) best-tests]
        [(= card max-card) (choose-test best-tests)]
        [else (select-test (+ card 1)
                           #:max-card max-card
                           #:test-list lst
                           #:mutant-card-table mutant-card)]))
