#lang racket

(require "../../bex/util/sql-db.rkt"
         db)

(define (get-num-mutants result-table-name)
  (query-value dbc
               (format "SELECT COUNT(*) from (SELECT DISTINCT mutant_module, mutation_index from ~a)"
                       result-table-name)))

(define (get-num-dead-mutants #:result-table-name result-table-name
                              #:test-suite-table-name test-suite-table-name
                              #:serialized-configuration configuration)
  (query-value
   dbc
   (apply
    format
    (cons
     "SELECT COUNT(*) from (SELECT DISTINCT mutant_module, mutation_index from ~a
                            INNER JOIN ~a
                            ON ~a.module_under_test = ~a.module_under_test AND
                               ~a.test_index = ~a.test_index
                            WHERE configuration = $1
                            AND test_passed = 0)"
     (build-list 6 (lambda (idx) (if (even? idx) result-table-name test-suite-table-name)))))
   configuration))

(define (get-mutation-score #:result-table-name result-table-name
                            #:test-suite-table-name test-suite-table-name
                            #:num-mutants [maybe-num-mutants #f]
                            #:serialized-configuration configuration)
  (define num-mutants (or maybe-num-mutants (get-num-mutants result-table-name)))
  (define num-dead-mutants
    (get-num-dead-mutants #:result-table-name result-table-name
                          #:test-suite-table-name test-suite-table-name
                          #:serialized-configuration configuration))
  (/ num-dead-mutants num-mutants))

(get-mutation-score #:result-table-name "forth"
                    #:test-suite-table-name "temp_test_suite"
                    #:serialized-configuration 2222)
