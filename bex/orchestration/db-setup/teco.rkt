#lang at-exp racket

(require "db-setup.rkt")

(db-setup-script #:mutation-analysis-config
                 "../../configurables/teco-configs/teco-prime.rkt"
                 #:mutation-analysis-error-type 'any-error
                 #:analyze-type-mutation-categories? #f
                 #:experiment-config-with-which-analyze-mutants-dynamic-errors
                 "../../configurables/teco-configs/teco-prime.rkt"
                 #:dynamic-error-filtering-lattice-config-id 'bot
                 #:dynamic-error-interestingness-filter? #f
                 #:search-for-interesting-scenarios? #f
                 #:mutants-to-sample-per-benchmark 'all
                 #:no-erasure-mode)
