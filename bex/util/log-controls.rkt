#lang at-exp racket

(provide (all-defined-out))

;; WARNING: enabling these controls causes double-logging if the PLTSTDOUT
;; and/or PLTSTDERR environment variables are configured. This is the case when
;; mutant-factory runs via standard-experiment-runner-script-template.sh.

;; On top of obvious readability issues, double-logging causes issues with
;; other parts of the experiment infrastructure that read these logs. For
;; example, check-experiment-results.rkt expects the last log line to contain
;; the line "Experiment complete, and basic sanity checks pass", which it will
;; not with double-logging.

(define-values (print-mutant-factory-logs? mutant-factory-log-level) (values #f 'info))
(define-values (print-mutant-util-logs? mutant-util-log-level) (values #f 'debug))
(define-values (print-mutant-runner-logs? mutant-runner-log-level) (values #f 'info))
(define-values (print-mutate-logs? mutate-log-level) (values #f 'info))
(define-values (print-mutation-runner-logs? mutation-runner-log-level) (values #f 'info))
(define-values (print-instrumented-runner-logs? instrumented-runner-log-level) (values #f 'info))
(define-values (print-teco-run-logs? teco-run-log-level) (values #f 'info))
