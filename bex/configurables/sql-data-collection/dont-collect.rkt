#lang at-exp racket

(require db
         "../../runner/mutation-runner-data.rkt"
         "../../configurations/config.rkt")

(provide (contract-out
          [ensure-db! (-> void?)]
          [connect-to-db (-> dummy-connection)]
          [ensure-table! (valid-table-name/c dummy-connection . -> . void?)]
          [add-table-entry! (valid-table-name/c
                             dummy-connection
                             #:configuration config/c
                             #:module-under-test string?
                             #:test-index natural-number/c
                             #:mutant-module string?
                             #:mutation-index natural-number/c
                             #:run-status (or/c run-status/c 'skipped)
                             #:cmd-line-args string?
                             . -> .
                             void?)]))

(define dummy-connection #f)

(define valid-table-name/c
  (and/c string?
         (λ (table-name)
           (regexp-match #rx"^[A-Za-z_][A-Za-z0-9_]*$"
                         table-name))))

;; Create the sqlite database and necessary directory structure if it doesn't
;; yet exist
(define (ensure-db!)
    (void))

;; create a connection to the database given the dbs dir for the experiment (for
;; example, `dbs:teco` in experiment-info.rkt)
(define (connect-to-db)
  dummy-connection)

(define (ensure-table! _table-name _dbc)
  (void))

(define (add-table-entry! _table-name _dbc
                          #:configuration _configuration
                          #:module-under-test _module-under-test
                          #:test-index _test-index
                          #:mutant-module _mutant-module
                          #:mutation-index _mutation-index
                          #:run-status _run-status
                          #:cmd-line-args _cmd-line-args)
  (void))

