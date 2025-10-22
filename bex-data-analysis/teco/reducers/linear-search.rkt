#lang racket

(require db
         "../calculate-teco-score.rkt"
         "../../../bex/util/sql-db.rkt"
         "../common.rkt")

(provide reduce-by-lin-search)

(define (reduce-by-lin-search #:test-suite suite
                              #:test-mutant-mapping mapping
                              #:configuration conf
                              #:get-total-order get-total-order
                              #:choose-test choose-test)

  (define (get-mscore suite)
    (get-mutation-score #:result-table-name mapping
                        #:test-suite-table-name suite
                        #:serialized-configuration conf))

  (define goal-score (get-mscore suite))

  (printf "[GOAL] ~a has mutation score ~a~n~n" suite goal-score)

  (define starting-point (format "~a_~a_~a" suite conf "linsearch_start"))

  (copy-table! #:src suite #:dest starting-point)

  (define winner
    (for/fold ([accum starting-point])
              ([counter (in-naturals)]
               [current-test (get-total-order)])

      (define new-suite
        (remove-test #:remove current-test
                     #:from accum
                     #:to-create (format "~a_~a_temp~a" suite conf (number->string counter))))

      (define new-score (get-mscore new-suite))

      (printf "~a has mutation score ~a~n~n" new-suite new-score)

      (cond
        [(= goal-score new-score)
         (drop-table! accum)
         new-suite]
        [else
         (drop-table! new-suite)
         accum])))

  (define result-name (format "~a_~a_linsearch_result" suite conf))
  (copy-table! #:src winner #:dest result-name)
  (drop-table! winner)
  result-name)

(define (remove-test #:remove test #:from suite #:to-create dest)
  (drop-table! dest)
  (query-exec
   dbc
   (format
    "CREATE TABLE ~a AS
    SELECT module_under_test, test_index FROM ~a
    WHERE module_under_test != $1 OR test_index != $2"
    dest
    suite)
   (test-id-modul test)
   (test-id-index test))
  dest)
