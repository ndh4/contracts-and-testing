#lang s-exp "experiment-lang.rkt"

(require "experiment-info.rkt")

(define-runtime-path status-file "../../../experiment-status.txt")

(with-configuration [nathaniel
                     teco]
  #:contract-levels max types
  #:only abm_test
  #:status-in status-file
  #:manual-outcome-recording
  (run-mode teco-prime #:record-outcomes))
