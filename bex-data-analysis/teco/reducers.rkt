#lang racket

(require db
         "../../bex/util/sql-db.rkt"
         "calculate-teco-score.rkt"
         "common.rkt"
         "reducers/linear-search.rkt"
         "reducers/greedy.rkt"
         "reducers/harrold.rkt")

(define (run-reducers red-list #:test-suite suite #:test-mutant-mapping mapping #:configuration conf)
  (define tests
    (map vec->test-id (query-rows dbc (format "SELECT module_under_test, test_index from ~a" suite))))

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
    (reducer #:test-suite suite
             #:test-mutant-mapping mapping
             #:configuration conf
             #:get-total-order get-total-order
             #:choose-test choose-test)))

(define (get-random-order tests)
  (do-with-seed 12345 (lambda () (shuffle tests))))

(define (do-with-seed seed func)
  (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
    (random-seed seed)
    (func)))

(for ([conf (list 0 2222 #;0 #;22222)]
      [bm (list "morsecode" "morsecode" #;"dungeon" #;"dungeon")])
  ;; PREREQ: ../../bex/util/make-test-suite.rkt
  (run-reducers (list reduce-by-harrold #;reduce-by-lin-search #;reduce-by-vanilla-greedy)
                #:test-suite (string-append bm "_tests")
                #:test-mutant-mapping bm
                #:configuration conf))
