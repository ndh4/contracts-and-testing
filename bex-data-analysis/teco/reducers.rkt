#lang racket

(require db
         "db-params.rkt"
         "calculate-teco-score.rkt"
         "common.rkt"
         "reducers/linear-search.rkt"
         "reducers/greedy.rkt"
         "reducers/harrold.rkt"
         "reducers/delayed-greedy.rkt"
         racket/runtime-path)

(define (run-reducers red-list #:test-suite suite #:test-mutant-mapping mapping #:configuration conf)
  (define tests
    (map vec->test-id (query-rows (dbc) (format "SELECT module_under_test, test_index from ~a" suite))))

  (printf
   "~n~a (~a contracts)~n"
   mapping
   (cond
     [(= conf 0) "no"]
     [(or (= conf 22) (= conf 2222) (= conf 22222) (= conf 222222) (= conf 2222222) (= conf 22222222))
      "full"]
     [else "some"])) ;; code smell, but who really cares
  (printf "Mutation score: ~a~n"
          (real->decimal-string (get-mutation-score #:result-table-name mapping
                                                    #:test-suite-table-name suite
                                                    #:serialized-configuration conf)
                                6))
  (printf "Test suite size: ~a~n" (length tests))

  (define ordering (get-random-order tests)) ; sequence of test-ids
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
                                (format "SELECT module_under_test, test_index from ~a"
                                        reduction-result))))

    #;(displayln (format "~a: ~a"
                         result-table
                         (get-mutation-score #:result-table-name mapping
                                             #:test-suite-table-name result-table
                                             #:serialized-configuration conf)))))

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

;; Alter slices as needed
(for ([a-slice (list (slice "teco-04-20-2026@16:23:46" "kcfa" 0)
      (slice "teco-04-20-2026@16:23:46" "kcfa" 2222222)
      (slice "teco-04-21-2026@23:27:58" "morsecode" 0)
      (slice "teco-04-21-2026@23:27:58" "morsecode" 2222)
      (slice "teco-04-25-2026@16:15:01" "forth" 0)
      (slice "teco-04-25-2026@16:15:01" "forth" 2222)
      (slice "teco-04-22-2026@10:43:21" "sieve" 0)
      (slice "teco-04-22-2026@10:43:21" "sieve" 22)
      (slice "teco-04-24-2026@20:59:51" "dungeon" 0)
      (slice "teco-04-24-2026@20:59:51" "dungeon" 22222)
      (slice "teco-04-24-2026@13:59:38" "snake" 0)
      (slice "teco-04-24-2026@13:59:38" "snake" 22222222)
      (slice "teco-04-23-2026@15:08:11" "mbta" 0)
      (slice "teco-04-23-2026@15:08:11" "mbta" 222222))])

  (define bm-name (slice-benchmark a-slice))

  (parameterize ([dbc (db-connection a-slice)])
    (maybe-make-test-suite! #:src bm-name #:dest (string-append bm-name "_tests"))
    (maybe-move-sanity! bm-name)
    (run-reducers
     (list reduce-by-lin-search reduce-by-vanilla-greedy reduce-by-delayed-greedy reduce-by-harrold)
     #:test-suite (string-append bm-name "_tests")
     #:test-mutant-mapping bm-name
     #:configuration (slice-config a-slice))))
