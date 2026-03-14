#lang at-exp racket

#;
(require db
         racket/runtime-path)

#;
(provide ensure-table!
         add-entry!
         bool->int
         dbc)

#;
(define-runtime-path db-path "../dbs/sqlite/teco2.sqlite3")

#;
(define dbc (sqlite3-connect #:database db-path #:mode 'create))

#;
(define (ensure-table! table-name)
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

#;
(define (add-entry! table-name
                    #:configuration configuration
                    #:module_under_test module_under_test
                    #:test_index test_index
                    #:mutant_module mutant_module
                    #:mutation_index mutation_index
                    #:test_passed test_passed
                    #:outcome outcome
                    #:blamed blamed
                    #:errortrace_stack errortrace_stack
                    #:context_stack context_stack
                    #:result_value result_value
                    #:cmd_line_args cmd_line_args)
  (query-exec
   dbc
   (format
    "INSERT OR REPLACE INTO ~a
                    (configuration, module_under_test, test_index, mutant_module, mutation_index, test_passed, outcome, blamed, errortrace_stack, context_stack, result_value, cmd_line_args)
                    VALUES($2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)"
    table-name)
   configuration
   module_under_test
   test_index
   mutant_module
   mutation_index
   test_passed
   outcome
   blamed
   errortrace_stack
   context_stack
   result_value
   cmd_line_args))

#;
(define (bool->int b)
  (if b 1 0))
