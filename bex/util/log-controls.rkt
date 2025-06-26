#lang at-exp racket

(provide (all-defined-out))

(define-values (print-mutant-factory-logs? mutant-factory-log-level) (values #t 'info))
(define-values (print-mutant-util-logs? mutant-util-log-level) (values #t 'info))
(define-values (print-mutant-runner-logs? mutant-runner-log-level) (values #t 'debug))
(define-values (print-mutate-logs? mutate-log-level) (values #f 'debug))
(define-values (print-mutation-runner-logs? mutation-runner-log-level) (values #f 'debug))
(define-values (print-instrumented-runner-logs? instrumented-runner-log-level) (values #f 'debug))
(define-values (print-teco-run-logs? teco-run-log-level) (values #f 'debug))
