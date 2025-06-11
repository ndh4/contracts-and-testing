#lang racket

(require "experiment-manager.rkt")

(define host zythos-ssh/one-job-per-mutant)
(define download-dir "/Users/nhejduk/Research-Local/blgt-parent/contracts-and-testing/bex/orchestration/../../../experiment-data/results/blutil")
(define name #f)
(define benchmark-names '(forth))

(download-results! host download-dir
                        #:name name
                        #:expected-benchmarks benchmark-names)