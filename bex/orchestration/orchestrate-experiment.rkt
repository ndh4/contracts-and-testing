#lang s-exp "experiment-lang.rkt"

(require "experiment-info.rkt")

(define-runtime-path status-file "../../../experiment-status.txt")

(with-configuration [local
                     teco]
  #:status-in status-file
  #:manual-outcome-recording
  (run-mode teco-prime #:only abm_test #:record-outcomes)
  #;(run-mode blame #:record-outcomes)
  #;(run-mode stack)
  #;(run-mode null))
