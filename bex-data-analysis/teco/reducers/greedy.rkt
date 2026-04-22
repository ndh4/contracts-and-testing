#lang racket

(require "../common.rkt")

(provide (contract-out [reduce-by-vanilla-greedy reducer/c]))

(define (reduce-by-vanilla-greedy #:test-suite suite
                                  #:test-mutant-mapping mapping
                                  #:configuration conf
                                  #:get-total-order get-total-order
                                  #:choose-test choose-test)

  (define kills-table
    (make-kills-table #:test-suite suite #:test-mutant-mapping mapping #:configuration conf))

  (define result-suite
    (make-result-suite #:start-suite suite #:configuration conf #:algo-name "vgreedy"))

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
       (test-chosen! #:test best-test #:kills-table kills-table)
       (loop)])))
