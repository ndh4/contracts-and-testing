#lang at-exp racket

(provide (all-defined-out)
         (struct-out orchestration-info))

(require racket/runtime-path
         (only-in srfi/19 date->string))

;; user-facing configuration for an orchestration
(struct orchestration-config (experiment-name setup-config db-setup-script)) 
;; built (timestamped) info for an orchestration
(struct orchestration-info (experiment-id setup-config db-setup-script)) 

;; timestamp a basic-orchestration-config with the current time, converting it
;; into an orchestration-config
(define (make-orchestration-info orch-cfg date)
  (define timestamp (date->string date "~m-~d-~Y@~T"))
  (define output-dir-name
    (format "~a-~a" (orchestration-config-experiment-name orch-cfg) timestamp))
  (orchestration-info output-dir-name
                      (orchestration-config-setup-config orch-cfg)
                      (orchestration-config-db-setup-script orch-cfg)))

;; setup scripts
(define setup:bltym "bltym-setup-config.rkt")
(define setup:blgt "blgt-setup-config.rkt")
(define setup:blutil "blutil-setup-config.rkt")
(define setup:teco "teco-setup-config.rkt")

;; db-setup scripts
(define db-setup:bltym "bltym.rkt")
(define db-setup:blgt "blgt.rkt")
(define db-setup:blutil "blutil.rkt")
(define db-setup:teco "teco.rkt")

;; relevant for the host _running the mutants_, not the local host
(define current-experiment-dir (make-parameter #f))

#; (define current-remote-host-db-installation-directory-name (make-parameter #f))

#; (define current-experiment-dbs-dir (make-parameter #f))

;; orchestration configs
(define type-mistakes
  (orchestration-config "type-api-mutations"
                        setup:bltym
                        db-setup:bltym))
(define code-mistakes
  (orchestration-config "code-mutations"
                        setup:blgt
                        db-setup:blgt))
(define blutil
  (orchestration-config "blutil"
                        setup:blutil
                        db-setup:blutil))
(define teco
  (orchestration-config "teco"
                        setup:teco
                        db-setup:teco))

;; All configs share the same output and benchmarks directories
(define-runtime-path benchmarks-dir "../../../gtp-benchmarks/benchmarks/")

(define scenario-samples-per-mutant 100)

;; FIXME these should not need to be commented out, but currently db-setup does
;; not respect #:only
(define experiment-benchmarks '("abm_test"
                                 "dungeon"
                                 "forth"
                                 "kcfa"
                                 "mbta"
                                 "morsecode"
                                 "sieve"
                                 "snake"
                                #;"telegram"
                                #;"bazaar"
                                #;"quirkle"
                                #;"ticket"))

(define experiment-modes '("teco-prime"))
