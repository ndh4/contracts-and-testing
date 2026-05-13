#lang racket

(require db
         "db-params.rkt"
         "calculate-teco-score.rkt"
         "common.rkt"
         "reduction-structs.rkt"
         "reductions-setup.rkt"
         "reducers/linear-search.rkt"
         "reducers/greedy.rkt"
         "reducers/harrold.rkt"
         "reducers/delayed-greedy.rkt"
         racket/runtime-path)

(define (run-reducers red-list #:test-suite suite #:test-mutant-mapping mapping #:configuration conf)
  (define tests
    (map vec->test-id (query-rows (dbc) (format "SELECT module_under_test, test_index, test_check_enabled from ~a ORDER BY module_under_test, test_index" suite))))

  (printf
   "~n~a (~a contracts, ~a)~n"
   mapping
   (cond
     [(= conf 0) "no"]
     [(or (= conf 11) (= conf 1111) (= conf 11111) (= conf 111111) (= conf 1111111) (= conf 11111111))
      "type-level"]
     [(or (= conf 22) (= conf 2222) (= conf 22222) (= conf 222222) (= conf 2222222) (= conf 22222222))
      "full"]
     [else "some"]) ;; code smell, but who really cares
  (match (tcc)
    ['yes_tc "test checks on"]
    ['no_tc "test checks off"]
    ['both_tc "test check mixed reduction"]))
  (printf "Mutation score: ~a~n"
          (real->decimal-string (get-mutation-score #:result-table-name mapping
                                                    #:test-suite-table-name suite
                                                    #:serialized-configuration conf)
                                6))
  (printf "Test suite size: ~a~n" (length tests))

  (define ordering (get-order tests)) ; sequence of test-ids
  (define ordering-map
    (for/hash ([test ordering]
               [idx (in-naturals)])
      (values test idx))) ; maps test-id to its position in `ordering`
  (define (get-total-order)
    ordering)
  ; Input: a list of test-ids
  ; Output: the test-id with lowest value in `ordering-map`
  (define (choose-test tests)
    (when (empty? tests)
      (error "Empty list passed to choose-test"))
    (first (foldl (lambda (elem result)
                    (define precedence (hash-ref ordering-map elem))
                    (if (< precedence (second result))
                        (list elem precedence)
                        result))
                  '(#f +inf.0)
                  tests)))

  (for/list ([reducer red-list])
    (define reduction-result
      (reducer #:test-suite suite
               #:test-mutant-mapping mapping
               #:configuration conf
               #:get-total-order get-total-order
               #:choose-test choose-test))

    (printf "Reduced test suite size: ~a~n"
            (length (query-rows (dbc)
                                (format "SELECT module_under_test, test_index, test_check_enabled from ~a"
                                        reduction-result))))

    #;(displayln (format "~a: ~a"
                         result-table
                         (get-mutation-score #:result-table-name mapping
                                             #:test-suite-table-name result-table
                                             #:serialized-configuration conf)))))

(define (get-order tests)
  (define-values (enabled disabled) (partition test-id-check-enabled? tests))
  (append (get-random-order disabled)
          (get-random-order enabled)))

(define (get-random-order tests)
  (do-with-seed 12345 (lambda () (shuffle tests))))

(define (do-with-seed seed func)
  (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
    (random-seed seed)
    (func)))

(define-runtime-path experiment-results "../../../experiment-results")

(define (db-connection a-slice)
  (define db-path (build-path experiment-results (slice-experiment a-slice) "experiment-output" "db.sqlite"))
  (sqlite3-connect #:database db-path #:mode 'read/write))

;; Run reducers
(for* ([a-slice slices-to-process]
       [test-check-config test-check-configs])

  (define bm-name (slice-benchmark a-slice))

  (parameterize ([dbc (db-connection a-slice)]
                 [tcc test-check-config])

    (define test-suite-name (format "~a_tests_~a" bm-name (tcc)))
    (maybe-make-test-suite! #:src bm-name #:dest test-suite-name)
    (maybe-move-sanity! bm-name)
    (run-reducers
     (list reduce-by-lin-search reduce-by-vanilla-greedy reduce-by-delayed-greedy reduce-by-harrold)
     #:test-suite test-suite-name
     #:test-mutant-mapping bm-name
     #:configuration (slice-config a-slice))))
