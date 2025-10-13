#lang racket

(require db
         "../../bex/util/sql-db.rkt"
         "calculate-teco-score.rkt"
         "common.rkt"
         "reducers/linear-search.rkt")

(define (run-reducers red-list #:test-suite suite #:test-mutant-mapping mapping #:configuration conf)
  (define tests
    (map (lambda (row) (make-test (vector-ref row 0) (vector-ref row 1)))
         (query-rows dbc (format "SELECT module_under_test, test_index from ~a" suite))))

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
    (when empty?
      tests
      (error "Empty list passed to choose-test"))
    (first (foldl (lambda (elem result)
                    (define precedence (hash-ref ordering-map elem))
                    (if (< precedence (second result))
                        (list elem precedence)
                        result))
                  '(#f +inf.0)
                  tests)))

  (for/list ([reducer red-list])
    (reducer #:test-suite suite
             #:test-mutant-mapping mapping
             #:configuration conf
             #:get-total-order get-total-order
             #:choose-test choose-test)))

(define (get-random-order tests)
  (random-seed 12345)
  ;; TODO return tests in a randomized order
  tests)

(run-reducers (list reduce-by-lin-search)
              #:test-suite "morsecode_tests"
              #:test-mutant-mapping "morsecode"
              #:configuration 0)
