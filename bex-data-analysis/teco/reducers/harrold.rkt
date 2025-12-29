#lang racket

(require db
         "../calculate-teco-score.rkt"
         "../../../bex/util/sql-db.rkt"
         "../common.rkt")

[provide reduce-by-harrold]

(define (wait modified-tbl)
  (displayln (format "Modified table ~a" modified-tbl))
  (display "Press ENTER to continue")
  (read-line))

;; a name to prefix all tables with for this reduction, just so that we don't
;; need to pass the table name dependencies to every helper
(define table-prefix-fn (make-parameter identity))

(define (reduce-by-harrold #:test-suite suite
                           #:test-mutant-mapping mapping
                           #:configuration conf
                           #:get-total-order get-total-order
                           #:choose-test choose-test)

  (parameterize ([table-prefix-fn
                  identity
                  #;(λ (table-name)
                    (format "~a_~a_harrold_~a" mapping conf table-name))])
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
    (kill-mutants! #:mutant-card-table mutant-card-table
                   #:test-list essential-tests
                   #:kills-table kills-table)
    (add-tests! #:to result-suite #:from essential-tests)
    
    (let card-loop ([cur-card 2])
      (define max-card (get-max-card #:mutant-card-table mutant-card-table))
      (displayln (format "cur-card: ~a, max-card: ~a" cur-card max-card))
      (when (cur-card . < . max-card)
        ;; add tests until all tests of this cardinality are killed
        (let add-tests-loop ()
          ;; list of tests killing unkilled mutants of cardinality CARD
          (define remaining-tests-for-cur-card
            (remaining-tests-for-card cur-card
                                      #:kills-table kills-table
                                      #:mutant-card-table mutant-card-table))
          (wait remaining-tests-for-cur-card)
          (unless (tbl-empty? remaining-tests-for-cur-card)
            ;; select the best test from the test list
            (define best-test
              (select-test cur-card
                           #:max-card max-card
                           #:choose-test choose-test
                           #:test-list remaining-tests-for-cur-card
                           #:mutant-card-table mutant-card-table
                           #:kills-table kills-table))
            (wait best-test)
            
            ;; mark any mutants killed by the selected test as killed
            (kill-mutants! #:mutant-card-table mutant-card-table
                           #:test-list best-test
                           #:kills-table kills-table)
            
            ;; add the selected test to result-table
            (add-tests! #:to result-suite #:from best-test)
            (wait result-suite)
            
            ;; continue adding tests
            (add-tests-loop)))

        ;; continue with the next cardinality
        (card-loop (+ cur-card 1))))))


(define (make-result-suite #:start-suite suite)
  (define new-name ((table-prefix-fn) "result"))
  (query-exec dbc (format "DROP TABLE IF EXISTS ~a" new-name))
  (query-exec dbc (format "CREATE TABLE ~a AS SELECT * FROM ~a WHERE FALSE" new-name suite))
  new-name)

(define (make-kills-table #:configuration conf #:test-suite suite #:test-mutant-mapping mapping)
  ; SQL SELECT [mutant-id] [test-id] [conf] WHERE test_passed = 0
  (define new-name ((table-prefix-fn) "kills"))
  (displayln new-name)
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
  (define new-name ((table-prefix-fn) "card"))
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
;; Test query:
;; SELECT * FROM "morsecode_0_harrold_TESTLIST_2" T
;; INNER JOIN "morsecode_0_harrold_kills" K
;; ON (T.module_under_test = K.module_under_test
;; AND T.test_index = K.test_index)
;; INNER JOIN "morsecode_0_harrold_card" C
;; ON K.mutant_module = C.mutant_module AND K.mutation_index = C.mutation_index

(define (remaining-tests-for-card card #:kills-table kills #:mutant-card-table mutant-card)
  (define new-name ((table-prefix-fn) (format "TESTLIST_~a" card)))
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
;;
;; WITH CardKillCounts AS (
;; SELECT List.module_under_test, List.test_index, COUNT(*) as count FROM "morsecode_0_harrold_TESTLIST_2" List
;; INNER JOIN "morsecode_0_harrold_kills" Kills
;;  ON (List.module_under_test=Kills.module_under_test AND
;;   List.test_index=Kills.test_index)
;; INNER JOIN (SELECT * FROM "morsecode_0_harrold_card" WHERE card = 2 AND killed = FALSE) MutantCard
;;  ON (Kills.mutant_module=MutantCard.mutant_module AND
;;     Kills.mutation_index=MutantCard.mutation_index)
;; GROUP BY List.module_under_test, List.test_index
;; )
;; SELECT module_under_test, test_index FROM CardKillCounts
;; WHERE count = (SELECT MAX(count) FROM CardKillCounts)
(define (best-tests-from-list-for-card card
                                       #:lst-table lst
                                       #:mutant-card-table mutant-card
                                       #:kills-table kills)
  (define new-name ((table-prefix-fn) (format "FILTER_~a" card)))
  (drop-table! new-name)
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

;; Add tests from TEST-LIST to TO-TABLE
(define (add-tests! #:to to-table #:from test-list)
  (query-exec
   dbc
   (format
    "INSERT INTO ~a
      SELECT * FROM ~a"
    to-table
    test-list)))

;; Mark all mutants from TEST-LIST as killed
;; verified to work, i believe:
;;  SELECT * FROM "morsecode_0_harrold_kills" K
;;  INNER JOIN "morsecode_0_harrold_card" C
;;  ON K.mutant_module = C.mutant_module AND K.mutation_index = C.mutation_index
;;  WHERE module_under_test = "levenshtein.rkt" AND (test_index = 8 OR test_index = 14) 
(define (kill-mutants! #:mutant-card-table mutant-card-table
                       #:test-list test-list
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
    test-list
    kills-table)))

;; convert a list of tests (SQL table) to a vector
(define (test-list->vec test-list)
  (query-rows
   dbc
   (format
    "SELECT module_under_test, test_index FROM ~a"
    test-list)))

;; Select the best test for cardinality CARD
(define (select-test card
                     #:max-card max-card
                     #:choose-test choose-test
                     #:test-list lst
                     #:mutant-card-table mutant-card
                     #:kills-table kills)
  (displayln (format "selecting test from ~a" lst))
  ;; Get the list of best tests for CARD
  (define best-tests
    (best-tests-from-list-for-card card
                                   #:lst-table lst
                                   #:mutant-card-table mutant-card
                                   #:kills-table kills))
  (wait best-tests)
  (displayln (format "found best tests ~a" best-tests))
  ;; If the list has multiple options, select the best option for the next
  ;; cardinality. If there is no next cardinality, choose by CHOOSE-TEST
  (cond [(= (tbl-size best-tests) 1) best-tests]
        [(= card max-card)
         (define best-tests-vec (best-tests->vec best-tests))
         (define best-tests (map vec->test-id best-tests-vec))
         (choose-test best-tests)]
        [else (select-test (+ card 1)
                           #:max-card max-card
                           #:choose-test choose-test
                           #:test-list best-tests
                           #:mutant-card-table mutant-card
                           #:kills-table kills)]))
