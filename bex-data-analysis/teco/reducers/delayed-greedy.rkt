#lang racket

(require db
         "../calculate-teco-score.rkt"
         "../../../bex/util/sql-db.rkt"
         "../common.rkt")

(provide (contract-out [reduce-by-delayed-greedy reducer/c]))

(define (reduce-by-delayed-greedy #:test-suite suite
                                  #:test-mutant-mapping mapping
                                  #:configuration conf
                                  #:get-total-order get-total-order
                                  #:choose-test choose-test)

  (define kills-table
    (make-kills-table #:test-suite suite #:test-mutant-mapping mapping #:configuration conf))

  (define result-suite
    (make-result-suite #:start-suite suite #:configuration conf #:algo-name "dgreedy"))

  (let loop ()
    (cond
      [(empty-table? kills-table)
       (drop-table! kills-table)
       result-suite]
      [else

       (do ()
           (not (or (object-reduce!? kills-table)
                    (attribute-reduce!? kills-table)
                    (owner-reduce!? #:kills-table kills-table #:result-suite result-suite))))

       (cond
         [(empty-table? kills-table)
          (drop-table! kills-table)
          result-suite]
         [else
          ;          (printf "[~a] Heuristic Use~n" suite)
          (one-greedy-step! #:kills-table kills-table
                            #:result-suite result-suite
                            #:choose-test choose-test)
          (loop)])])))

(define (one-greedy-step! #:kills-table kills-table
                          #:result-suite result-suite
                          #:choose-test choose-test)

  ; let best-tests be the set of tests with max aura
  (define best-tests-vec (get-best-tests #:kills-table kills-table))
  (define best-tests (map vec->test-id best-tests-vec))

  ; call choose-test on best-tests to get t
  (define best-test (choose-test best-tests))

  ; add t to result suite
  (add-test! #:test best-test #:suite result-suite)

  ; remove every mutant that t kills from kills-table
  (test-chosen! #:test best-test #:kills-table kills-table))

(define (object-reduce!? kills-table)
  (define query-result
    (query-rows
     dbc
     (format
      "SELECT
                 *
             FROM
                 (SELECT DISTINCT module_under_test, test_index FROM ~a) AS T1
             WHERE EXISTS
                 (SELECT
                      *
                  FROM
                      (SELECT DISTINCT module_under_test, test_index FROM ~a) AS T2
                  WHERE
                      T1.module_under_test <> T2.module_under_test AND T1.test_index <> T2.test_index
                    AND NOT EXISTS
                        (SELECT
                             *
                         FROM
                             ~a AS M1
                         WHERE
                             T1.module_under_test = M1.module_under_test AND T1.test_index = M1.test_index -- t1 kills m1
                            AND NOT EXISTS
                                (SELECT
                                     *
                                 FROM
                                     ~a AS M2
                                 WHERE
                                     M1.mutant_module = M2.mutant_module AND M1.mutation_index = M2.mutation_index -- m1 = m2
                                    AND
                                     T2.module_under_test = M2.module_under_test AND T2.test_index = M2.test_index -- t2 kills m2
                                    )
                         )
                  ) LIMIT 1"
      kills-table
      kills-table
      kills-table
      kills-table)))
  (cond
    [(empty? query-result) #f]
    [else
     (for ([row query-result])
       ;; We need to avoid cases where both sides of an =-subsumption get dropped
       ;; Right now, we're doing this with LIMIT 1. There might be a smarter way though.
       (define tst-id (vec->test-id row))
       ;       (printf "Object reduction found!~n")
       (delete-test! #:test tst-id #:kills-table kills-table))
     ;; Try to reduce more, but return true no matter what
     (object-reduce!? kills-table)
     #t]))

(define (attribute-reduce!? kills-table)
  (define query-result
    (query-rows
     dbc
     (format
      "SELECT
           *
       FROM
           (SELECT DISTINCT mutant_module, mutation_index FROM ~a) AS M2
       WHERE EXISTS
                 (SELECT
                      *
                  FROM
                      (SELECT DISTINCT mutant_module, mutation_index FROM ~a) AS M1
                  WHERE
                      M2.mutant_module <> M1.mutant_module AND M2.mutation_index <> M1.mutation_index
                    AND NOT EXISTS
                      (SELECT
                           *
                       FROM
                           ~a AS T1
                       WHERE
                           T1.mutant_module = M1.mutant_module AND T1.mutation_index = M1.mutation_index -- t1 kills m1
                         AND NOT EXISTS
                           (SELECT
                                *
                            FROM
                                ~a AS T2
                            WHERE
                                T1.module_under_test = T2.module_under_test AND T1.test_index = T2.test_index -- t1 = t2
                              AND
                                T2.mutant_module = M2.mutant_module AND T2.mutation_index = M2.mutation_index -- t2 kills m2
                           )
                      )
                 ) LIMIT 1"
      kills-table
      kills-table
      kills-table
      kills-table)))
  (cond
    [(empty? query-result) #f]
    [else
     (for ([row query-result])
       ;; We need to avoid cases where both sides of an =-subsumption get dropped
       ;; Right now, we're doing this with LIMIT 1. There might be a smarter way though.
       (define mut-id (vec->mutant-id row))
       ;       (printf "Attribute reduction found!~n")
       (delete-mutant! #:mutant mut-id #:kills-table kills-table))
     ;; Try to reduce more, but return true no matter what
     (attribute-reduce!? kills-table)
     #t]))

(define (owner-reduce!? #:kills-table kills-table #:result-suite result-suite)
  (define query-result
    (query-rows
     dbc
     (format
      "SELECT DISTINCT
           module_under_test, test_index
       FROM
           ~a AS table1
       WHERE
           (SELECT COUNT(*)
            FROM ~a AS table2
            WHERE table2.mutant_module = table1.mutant_module
              AND table2.mutation_index = table1.mutation_index)
               = 1"
      kills-table
      kills-table)))
  (cond
    [(empty? query-result) #f]
    [else
     (for ([row query-result])
       ;; We need to avoid cases where both sides of an =-subsumption get dropped
       ;; Right now, we're doing this with LIMIT 1. There might be a smarter way though.
       (define tst-id (vec->test-id row))
       ;       (printf "Owner reduction found!~n")

       ; add t to result suite
       (add-test! #:test tst-id #:suite result-suite)

       ; remove every mutant that t kills from kills-table
       (test-chosen! #:test tst-id #:kills-table kills-table))
     #t]))
