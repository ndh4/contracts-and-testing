#lang racket


(require racket/runtime-path
         "../configurations/configure-benchmark.rkt"
         "../util/path-utils.rkt")

(provide (contract-out
          [make-mutant-runner-script-args
           ({benchmark-configuration/c
             module-name?
             natural?
             path-string?
             path-string?
             #:fake-mutation? boolean?}
            {#:timeout/s (or/c #f number?)
             #:test-id (or/c #f number?)
             #:memory/gb (or/c #f number?)
             #:fake-run? boolean?
             #:log-mutation-info? boolean?
             #:save-output (or/c #f path-string?)
             #:write-modules-to (or/c #f path-string?)
             #:force-module-write? boolean?}
            . ->* .
            (listof string?))]))

(define-runtime-path mutant-runner-path "../experiment/mutant-runner.rkt")


(define (make-mutant-runner-script-args a-benchmark-configuration
                                        module-to-mutate
                                        mutation-index
                                        config-path
                                        current-experiment-dir

                                        #:fake-mutation? fake-mutation?
                                        #:fake-run? [fake-run? #f]
                                        #:log-mutation-info? [log-mutation-info? #f]
                                        #:test-id [test-id #f]
                                        #:timeout/s [timeout/s #f]
                                        #:memory/gb [memory/gb #f]
                                        #:save-output [output-path #f]

                                        #:write-modules-to [dump-dir-path #f]
                                        #:force-module-write? [force-module-write? #f])
  (append
   (if log-mutation-info?
       (list "-O" "info@mutate")
       empty)
   (list "--"
         (~a (simple-form-path mutant-runner-path))
         "-x" (~a (simple-form-path current-experiment-dir))
         "-b" (serialize-benchmark-configuration a-benchmark-configuration)
         "-T" (~a test-id)
         "-M" (~a module-to-mutate)
         "-i" (~a mutation-index)
         "-t" (~a timeout/s)
         "-g" (~a memory/gb)
         "-c" (~a (simple-form-path config-path)))
   (if fake-mutation?
       (list "-z")
       empty)
   (if fake-run?
       (list "--fake-run")
       empty)
   (if output-path
       (list "-O" (~a (simple-form-path output-path)))
       empty)
   (if dump-dir-path
       (list "-w" (~a (simple-form-path dump-dir-path)))
       empty)
   (if force-module-write?
       '("-f")
       empty)))
