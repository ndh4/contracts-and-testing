#lang s-exp "experiment-lang.rkt"

(require "experiment-info.rkt")

(define-runtime-path status-file "../../../experiment-status.txt")

(with-configuration [local
                     teco]
  #:contract-levels max types none
  #:test-types "hand" "rand"
  #:only abm_test
  #:status-in status-file
  #:manual-outcome-recording
  (run-mode teco-prime #:record-outcomes))
