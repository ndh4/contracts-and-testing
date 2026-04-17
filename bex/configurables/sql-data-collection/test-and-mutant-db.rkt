#lang at-exp racket

(require db
         "../../util/experiment-exns.rkt"
         "../../configurables/configurables.rkt"
         "../../runner/mutation-runner-data.rkt"
         "../../configurations/config.rkt"
         )

(provide (contract-out
          [db-path (parameter/c (or/c path-to-db/c #f))]
          [ensure-db! (-> void?)]
          [connect-to-db (->* ()
                              #:pre (λ () (path-to-db/c (db-path)))
                              connection?)]
          [ensure-table! (valid-table-name/c connection? . -> . void?)]
          [add-table-entry! (valid-table-name/c
                             connection?
                             #:configuration config/c
                             #:module-under-test string?
                             #:test-index natural-number/c
                             #:mutant-module string?
                             #:mutation-index natural-number/c
                             #:run-status (or/c run-status/c 'skipped)
                             #:cmd-line-args string?
                             . -> .
                             void?)]))

(define path-to-db/c
  (and/c absolute-path?
         (λ (p) path-has-extension? p ".sqlite3")))

(define valid-table-name/c
  (and/c string?
         (λ (table-name)
           (regexp-match #rx"^[A-Za-z_][A-Za-z0-9_]*$"
                         table-name))))

(define db-path (make-parameter #f))

;; Create the sqlite database and necessary directory structure if it doesn't
;; yet exist
(define (ensure-db!)
  (define cur-db-path (db-path))
  (unless (file-exists? cur-db-path)
    (define-values (db-dir _file-name _) (split-path cur-db-path))
    (make-directory* db-dir)
    ;; NOTE this connection is ignored. Is there a better way to create an empty
    ;; sqlite db?
    (sqlite3-connect #:database cur-db-path #:mode 'create) 
    (void)))

;; create a connection to the database given the dbs dir for the experiment (for
;; example, `dbs:teco` in experiment-info.rkt)
(define (connect-to-db)
  (define cur-db-path (db-path))
  (unless (file-exists? cur-db-path)
    (define msg
      (format "~a does not point to a valid sqlite db. Did you remember to generate dbs with `bex/orchestration/db-setup/[experiment].rkt`?"
              cur-db-path))
    (raise-experiment-user-error 'connect-to-db msg))
  (sqlite3-connect #:database (db-path)))

(define (ensure-table! table-name dbc)
  (query-exec
   dbc
   (format
    "CREATE TABLE IF NOT EXISTS ~a (
  configuration INTEGER,
  module_under_test TEXT,
  test_index INTEGER,
  mutant_module TEXT,
  mutation_index INTEGER,
  test_passed INTEGER,
  outcome TEXT,
  blamed TEXT,
  errortrace_stack TEXT,
  context_stack TEXT,
  result_value TEXT,
  cmd_line_args TEXT,
  PRIMARY KEY (configuration, module_under_test, test_index, mutant_module, mutation_index)
  ON CONFLICT REPLACE
  )"
    table-name)))

(define (add-table-entry! table-name dbc
                          #:configuration configuration
                          #:module-under-test module-under-test
                          #:test-index test-index
                          #:mutant-module mutant-module
                          #:mutation-index mutation-index
                          #:run-status run-status
                          #:cmd-line-args cmd-line-args)
  ;; stringify the results from the run status
  (define-values
    (test-passed
     outcome
     blamed
     errortrace-stack
     context-stack
     result-value)
    (cond [(run-status? run-status)
           (values (eq? (run-status-outcome run-status) 'completed)
                   (~a (run-status-outcome run-status))
                   (~a (run-status-blamed run-status))
                   (~a (run-status-errortrace-stack run-status))
                   (~a (run-status-context-stack run-status))
                   (~a (run-status-result-value run-status)))]
          [(eq? 'skipped run-status)
           (values #t
                   "skipped"
                   (~a #f)
                   (~a #f)
                   (~a #f)
                   (~a #f))]))
  (query-exec
   dbc
   (format
    "INSERT OR REPLACE INTO ~a
                    (configuration, module_under_test, test_index, mutant_module, mutation_index, test_passed, outcome, blamed, errortrace_stack, context_stack, result_value, cmd_line_args)
                    VALUES($2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)"
    table-name)

   ((configured:serialize-config) configuration)
   module-under-test
   test-index
   mutant-module
   mutation-index
   (bool->int test-passed)
   outcome
   blamed
   errortrace-stack
   context-stack
   result-value
   cmd-line-args))

(define/contract (bool->int b)
  (-> boolean? integer?)
  (if b 1 0))
