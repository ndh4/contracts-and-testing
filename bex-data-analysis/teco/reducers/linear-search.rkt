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

  (for/fold ([accum suite])
            ([counter (in-naturals)]
             [current-test (get-total-order)])

    (define new-suite
      (remove-test #:remove current-test
                   #:from accum
                   #:to-create (string-append suite (number->string counter))))

    (define new-score (get-mscore new-suite))

    (printf "~a has mutation score ~a~n~n" new-suite new-score)

    (if (= goal-score new-score) new-suite accum)))

(define (remove-test #:remove test #:from suite #:to-create new-name)
  (query-exec
   dbc
   (format
    "CREATE TABLE ~a AS
    SELECT module_under_test, test_index from ~a
    WHERE module_under_test != $1 OR test_index != $2"
    new-name
    suite)
   (test-modul test)
   (test-index test))
  new-name)
