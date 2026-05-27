#lang at-exp racket/base

(require racket/list
         racket/match
         racket/sequence
         racket/string
         racket/cmdline
         racket/port
         racket/format
         racket/runtime-path
         racket/path
         racket/function
;         bex/configurables/configurables
         "../runner/mutation-runner.rkt"
         "../runner/unify-program.rkt"
         "../util/program.rkt"
         "../util/log-controls.rkt"
         "../util/path-utils.rkt"
         "../configurables/configurables.rkt"
         "../configurations/configure-benchmark.rkt"
         "../orchestration/experiment-info.rkt")

(define (fail fmt-str . fmt-args)
  (apply eprintf
         (string-append fmt-str "\n")
         fmt-args)
  (exit 1))

(define-logger mutant-runner)

(define recvr (make-log-receiver mutant-runner-logger mutant-runner-log-level))

(when print-mutant-runner-logs?
 (void (thread (lambda ()
                 (let loop ()
                   (define v (sync recvr))
                   (printf "[~a] ~a~n" (vector-ref v 0) (vector-ref v 1))
                   (loop))))))

(module+ main
  (define the-benchmark-configuration (make-parameter #f))
  (define module-to-mutate (make-parameter #f))
  (define mutation-index (make-parameter #f))
  (define test-id (make-parameter #f))
  (define write-modules-to (make-parameter #f))
  (define on-module-exists (make-parameter 'error))
  (define timeout/s (make-parameter #f))
  (define memory/gb (make-parameter #f))
  (define mutant-output-path (make-parameter #f))
  (define configuration-path (make-parameter #f))
  (define fake-mutation? (make-parameter #f))
  (define fake-run? (make-parameter #f))

  (command-line
   #:once-each
   [("-x" "--experiment-dir")
    dir
    "Directory for this experiment. Mandatory."
    (current-experiment-dir dir)]
   [("-b" "--benchmark-configuration")
    benchmark-configuration-str
    ("`write` form of the `benchmark-configuration` to run."
     "This is a mandatory argument.")
    (with-handlers ([exn:fail:read?
                     (λ _
                       (fail
                        @~a{
                            Unable to read benchmark configuration
                            Provided: @~v[benchmark-configuration-str]}))])
      (the-benchmark-configuration (with-input-from-string benchmark-configuration-str read)))]
   [("-T" "--test-id")
    t-index
    ("Test index."
     "This is a mandatory argument.")
    (test-id (string->number t-index))]
   [("-M" "--module-to-mutate")
    mutant-mod
    ("Name of the module to mutate."
     "This is a mandatory argument.")
    (module-to-mutate mutant-mod)]
   [("-i" "--mutation-index")
    m-index
    ("Mutation index."
     "This is a mandatory argument.")
    (mutation-index (string->number m-index))]
   [("-w" "--write-modules-to")
    output-path
    "Path to output mutated modules to"
    (write-modules-to output-path)]
   [("-f" "--overwrite-modules")
    "When writing modules, overwrite files that already exist"
    (on-module-exists 'replace)]
   [("-t" "--timeout")
    seconds
    "Timeout limit (seconds)"
    (timeout/s (string->number seconds))]
   [("-g" "--memory-limit")
    gb
    "Memory limit (GB)"
    (memory/gb (string->number gb))]

   [("-O" "--output")
    path
    "Send mutant output to `path` instead of suppressing it."
    (mutant-output-path path)]

   [("-c" "--config")
    path
    ("The configuration with which to run the mutant."
     "This is a mandatory argument.")
    (configuration-path path)]

   [("-z" "--fake-mutation")
    "Is this a sanity check with no actual mutation?"
    (fake-mutation? #t)]

   [("--fake-run")
    "Is this a fake run for the purpose of inital mutant aggregation?"
    (fake-run? #t)])

  (define mutant-output-path-port
    (match (mutant-output-path)
      [#f #f]
      [path
       (define port (open-output-file #:exists 'replace path))
       (file-stream-buffer-mode port 'line)
       port]))
  (define (mutant-output-or get-port)
    (or mutant-output-path-port
        (get-port)))

  (define missing-arg
    (for/first ([arg (in-list (list (current-experiment-dir)
                                    (the-benchmark-configuration)
                                    (module-to-mutate)
                                    (mutation-index)
                                    (configuration-path)))]
                [flag (in-list '(-x -b -M -i -c))]
                #:unless arg)
      flag))

  (when missing-arg
    (fail
     @~a{Error: Missing mandatory argument: @missing-arg}))

  (install-configuration! (configuration-path))

  (define the-program
    (unify-program-for-running
     ((configured:benchmark-configuration->program)
      (the-benchmark-configuration))))

  (define the-program-mods (program->mods the-program))

  (define module-to-mutate-path
    (resolve-configured-benchmark-module (the-benchmark-configuration)
                                         (module-to-mutate)))

  (cond [module-to-mutate-path

  (define the-module-to-mutate
    (find-unified-module-to-mutate module-to-mutate-path
                                   the-program-mods))

  (define the-run-status
      (parameterize ([current-output-port (mutant-output-or current-output-port)]
                     [current-error-port  (mutant-output-or current-error-port)]
                     [current-mutated-program-exn-recordor
                      (and mutant-output-path-port
                           (λ (e) ((error-display-handler) (exn-message e) e)))])
        (run-with-mutated-module
         the-program
         the-module-to-mutate
         (mutation-index)
         (benchmark-configuration-config (the-benchmark-configuration))
         #:test-id (test-id)
         #:timeout/s (timeout/s)
         #:memory/gb (memory/gb)
         #:modules-base-path (find-program-base-path the-program)
         #:write-modules-to (write-modules-to)
         #:on-module-exists (on-module-exists)
         #:fake-mutation? (fake-mutation?)
         #:fake-run? (fake-run?)
         #:suppress-output? (not (mutant-output-path)))))
  (when mutant-output-path-port
    (close-output-port mutant-output-path-port))

  (writeln the-run-status)]

   [else
   ;; The mutant module is not the module under test,
   ;; nor is the mutant module a dependency of the module under test.
   ;; So, we assume that the test passes.

;(printf "Mutant-module is ~a~n" (path->string (file-name-from-path (module-to-mutate))))
   (define the-run-status
   (run-status (module-to-mutate)
              (mutation-index) #f 'skipped #f #f #f #f #f))

  (writeln the-run-status)
   ]))

(define (resolve-configured-benchmark-module a-benchmark-configuration
                                          a-module-name)
   (findf (path-ends-with a-module-name)
      (list* (benchmark-configuration-main a-benchmark-configuration)
          (benchmark-configuration-others a-benchmark-configuration))))


(module+ test
  (require ruinit
           racket
           "../configurables/configurables.rkt"
           racket/runtime-path)

  (define-runtime-path test-config "../configurables/configs/test.rkt")
  (install-configuration! (simple-form-path test-config))

  (define-test-env {setup! cleanup!}
    #:directories ([test-bench "./test-bench"]
                   [ut "./test-bench/untyped"]
                   [t  "./test-bench/typed"])
    #:files ([test-module-1 (build-path ut "test-mod-1.rkt")
                           @~a{
                               #lang racket

                               (require "test-mod-2.rkt")

                               (define (baz x y)
                                 (if (even? x)
                                     y
                                     (/ y x)))

                               (foo (baz 0 1))
                               }]
             [test-module-2 (build-path t "test-mod-2.rkt")
                           @~a{
                               #lang typed/racket

                               (provide foo)

                               (: foo (-> Number Number))
                               (define (foo x)
                                 (+ x x))
                               }]))
  (test-begin
    #:name mutant-runner
    #:short-circuit
    #:before (setup!)
    #:after (cleanup!)

    (ignore
     (define test-program (make-unified-program test-module-1
                                                (list test-module-2)))
     (define to-mutate
       (find-unified-module-to-mutate test-module-1
                                      (program->mods test-program))))

    (or (run-with-mutated-module
         test-program
         to-mutate
         0
         (hash))
        #t)))
