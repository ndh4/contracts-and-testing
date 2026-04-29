#lang racket

(require db
         "db-params.rkt")

(provide get-mutation-score)

(define (printx x)
  (printf "~a~n" x)
  x)

(define (get-num-mutants result-table-name)
  (query-value (dbc)
               (format "SELECT COUNT(*) from (SELECT DISTINCT mutant_module, mutation_index from ~a)"
                       result-table-name)))

(define (get-num-dead-mutants #:result-table-name result-table-name
                              #:test-suite-table-name test-suite-table-name
                              #:serialized-configuration configuration)
  (query-value
   (dbc)
   (apply
    format
    (cons
     "SELECT COUNT(*) from (SELECT DISTINCT mutant_module, mutation_index from ~a
                            INNER JOIN ~a
                            ON ~a.module_under_test = ~a.module_under_test AND
                               ~a.test_index = ~a.test_index
                            WHERE configuration = $1
                            AND ~a)"
     (append (build-list 6 (lambda (idx) (if (even? idx) result-table-name test-suite-table-name)))
             (list test-passed=0))))
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

(define (get-mscore-from-config config benchmark-name)
  (get-mutation-score #:result-table-name benchmark-name
                      #:test-suite-table-name (string-append benchmark-name "_tests")
                      #:serialized-configuration config))

(define (get-all-configs benchmark-name)
  (query-list (dbc) (format "SELECT configuration from ~a GROUP BY configuration" benchmark-name)))

(define (print-all-mscores benchmark-name)
  (define configs (get-all-configs benchmark-name))
  (for ([config configs])
    (printf "Mutation score for ~a configuration: ~a~n"
            config
            (get-mscore-from-config config benchmark-name))))
