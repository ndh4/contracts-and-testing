#lang racket

(require db
         "../calculate-teco-score.rkt"
         "../../../bex/util/sql-db.rkt"
         "../common.rkt")

[provide reduce-by-harrold]

;; A hash-set containing the names of all tables introduced during this
;; reduction so that we can clean up at the end
(define created-tables (make-parameter null))

(define (record-new-table! tbl)
  (set-add! (created-tables) tbl))

(define (clean-up-tables!)
  (for ([tab (set->list (created-tables))])
    (drop-table! tab)))

;; A string to prefix all tables with for this reduction, just so that we
;; don't need to pass the table name dependencies to every helper
(define table-prefix (make-parameter null))

(define (add-prefix table-name)
  (string-append (table-prefix) table-name))

;; Prepare NAME for use as a table name. Completes the following steps:
;; - Formats NAME with table-prefix
;; - Drops any existing table with the formatted name
;; - Adds the formatted name to CREATED-TABLES
;; - Returns the formatted name
;; string -> string
(define (prepare-table-name! name #:is-helper (is-helper #t))
  (let ([formatted-name (add-prefix name)])
    (drop-table! formatted-name)
    (when is-helper (record-new-table! formatted-name))
    formatted-name))

;; Reduce SUITE by the Harrold algorithm
;; table table nat (-> (listof test-id)) ((listof test-id) -> test-id) -> table
(define (reduce-by-harrold #:test-suite suite
                           #:test-mutant-mapping mapping
                           #:configuration conf
                           #:get-total-order get-total-order
                           #:choose-test choose-test)

  (parameterize ([created-tables (mutable-set)]
                 [table-prefix (format "~a_~a_harrold_" suite conf)])
    ;; "kills" binary relation
    (define kills-table
      (make-kills-table #:test-suite suite
                        #:test-mutant-mapping mapping
                        #:configuration conf))
    
    ;; mapping from mutants to cardinality / killed?
    (define mutant-card-table
      (make-mutant-card-table
       #:kills-table kills-table))
    
    ;; table of results
    (define result-suite (make-result-suite #:start-suite suite))
    
    ;; begin with all tests that kill mutants of cardinality 1, kill their
    ;; corresponding mutants, and add the tests to the result suite
    (define essential-tests
      (remaining-tests-for-card 1
                                #:kills-table kills-table
                                #:mutant-card-table mutant-card-table))
    (kill-mutants/testlist! #:mutant-card-table mutant-card-table
                            #:testlist essential-tests
                            #:kills-table kills-table)
    (add-tests! #:to result-suite #:from essential-tests)
    
    (let card-loop ([cur-card 2])
      (define max-card (get-max-card #:mutant-card-table mutant-card-table))
      (displayln (format "assessing cardinality ~a (max-card: ~a)" cur-card max-card))
      (when (cur-card . <= . max-card)
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
                           #:testlist remaining-tests-for-cur-card
                           #:mutant-card-table mutant-card-table
                           #:kills-table kills-table))
            
            (displayln (format "selected test (~a, ~a)"
                               (test-id-modul best-test)
                               (test-id-index best-test)))
            ;; mark any mutants killed by the selected test as killed
            (kill-mutants/test-id! #:mutant-card-table mutant-card-table
                                   #:test-id best-test
                                   #:kills-table kills-table)
            
            ;; add the selected test to result-table
            (add-test! #:suite result-suite #:test best-test)
            
            ;; continue adding tests
            (add-tests-loop)))

        ;; continue with the next cardinality
        (card-loop (+ cur-card 1))))

    ;; clean up tables created during this reduction
    (clean-up-tables!)

    ;; return the result suite
    result-suite))

;; Create the table that holds all of the tests needed for the final suite
;; table -> table
(define (make-result-suite #:start-suite suite)
  (define new-name (prepare-table-name! "result" #:is-helper #f))
  (query-exec dbc (format "DROP TABLE IF EXISTS ~a" new-name))
  (query-exec dbc (format "CREATE TABLE ~a AS SELECT * FROM ~a WHERE FALSE" new-name suite))
  new-name)

;; Create a table with information on each mutant, including cardinality and
;; whether it is killed by the existing tests in the result table
;; table -> table
(define (make-mutant-card-table #:kills-table kills)
  (define new-name (prepare-table-name! "card"))
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
;; table -> nat
(define (get-max-card #:mutant-card-table mutant-card)
  (define card
    (query-value
     dbc
     (format
      "SELECT MAX(card) FROM ~a
       WHERE killed = FALSE"
      mutant-card)))
  (if (sql-null? card) 0 card))

;; query the number of rows in TBL
;; table -> nat
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
;; nat table... -> table
(define (remaining-tests-for-card card #:kills-table kills #:mutant-card-table mutant-card)
  (define new-name (prepare-table-name! (format "TESTLIST_~a" card)))
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
;; nat table... -> table
(define (best-tests-from-list-for-card card
                                       #:lst-table lst
                                       #:mutant-card-table mutant-card
                                       #:kills-table kills)
  (define new-name (prepare-table-name! (format "FILTER_~a" card)))
  (query-exec
   dbc
   (format
    "CREATE TABLE ~a AS
      WITH CardKillCounts AS (
       SELECT Kills.module_under_test, Kills.test_index, COUNT(*) as count FROM ~a Kills
       INNER JOIN (SELECT * FROM ~a WHERE card = $1 AND killed = FALSE) MutantCard
        ON (Kills.mutant_module=MutantCard.mutant_module AND
            Kills.mutation_index=MutantCard.mutation_index)
       GROUP BY Kills.module_under_test, Kills.test_index
      ),
      ListCardKillCounts AS (
       SELECT List.module_under_test, List.test_index, COALESCE(count, 0) AS count FROM CardKillCounts
       RIGHT JOIN ~a List
       ON (List.module_under_test=CardKillCounts.module_under_test AND
           List.test_index=CardKillCounts.test_index)
      )
      SELECT module_under_test, test_index FROM ListCardKillCounts
      WHERE count=(SELECT COALESCE(MAX(count), 0) FROM CardKillCounts)"
    new-name
    kills
    mutant-card
    lst)
   card)
  new-name)

;; Add tests from TESTLIST to TO-TABLE
;; table... -> void
(define (add-tests! #:to to-table #:from testlist)
  (query-exec
   dbc
   (format
    "INSERT INTO ~a
      SELECT * FROM ~a"
    to-table
    testlist)))

;; Mark all mutants killed by tests in TESTLIST as killed
;; table... -> void
(define (kill-mutants/testlist! #:mutant-card-table mutant-card-table
                                #:testlist testlist
                                #:kills-table kills-table)
  (query-exec
   dbc
   (format
    "UPDATE ~a AS MutantCard
      SET killed = TRUE
      WHERE EXISTS (
        SELECT 1
        FROM ~a as List
        LEFT JOIN ~a Kills
         ON (List.module_under_test = Kills.module_under_test AND
             List.test_index = Kills.test_index)
        WHERE (Kills.mutant_module=MutantCard.mutant_module AND
               Kills.mutation_index=MutantCard.mutation_index)
      )"
    mutant-card-table
    testlist
    kills-table)))

;; Mark the mutants killed by test TEST-ID as killed
;; table... -> void
(define (kill-mutants/test-id! #:mutant-card-table mutant-card-table
                               #:test-id test-id
                               #:kills-table kills-table)
  (query-exec
   dbc
   (format
    "UPDATE ~a AS MutantCard
      SET killed = TRUE
      WHERE EXISTS (
        SELECT 1
        FROM ~a Kills
        WHERE (Kills.mutant_module=MutantCard.mutant_module AND
               Kills.mutation_index=MutantCard.mutation_index AND
               Kills.test_index=~a AND
               Kills.module_under_test=\"~a\")
      )"
    mutant-card-table
    kills-table
    (test-id-index test-id)
    (test-id-modul test-id))))

;; convert a testlist (SQL table) to a list of test IDs
;; table -> (listof test-id)
(define (testlist->test-ids testlist)
  (map vec->test-id
       (query-rows
        dbc
        (format
         "SELECT module_under_test, test_index FROM ~a"
         testlist))))

;; Select the best test for cardinality CARD. Returns a test ID
;; nat table... -> test-id
(define (select-test card
                     #:max-card max-card
                     #:choose-test choose-test
                     #:testlist lst
                     #:mutant-card-table mutant-card
                     #:kills-table kills)
  ;; Get the list of best tests for CARD
  (define best-tests
    (best-tests-from-list-for-card card
                                   #:lst-table lst
                                   #:mutant-card-table mutant-card
                                   #:kills-table kills))
  ;; If the list has multiple options, select the best option for the next
  ;; cardinality. If there is no next cardinality, choose by CHOOSE-TEST
  (cond [(= (tbl-size best-tests) 1)
         (first (testlist->test-ids best-tests))]
        [(= card max-card)
         (choose-test (testlist->test-ids best-tests))]
        [else (select-test (+ card 1)
                           #:max-card max-card
                           #:choose-test choose-test
                           #:testlist best-tests
                           #:mutant-card-table mutant-card
                           #:kills-table kills)]))
