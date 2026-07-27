#lang at-exp racket

(require "../runner/mutation-runner.rkt"
         (only-in "../runner/mutation-runner-data.rkt" run-outcome/c)
         "../runner/unify-program.rkt"
         "../util/program.rkt"
         "../util/path-utils.rkt"
         "../util/read-module.rkt"
         "../util/optional-contracts.rkt"
         "../util/mutant-util.rkt"
         "../util/progress-log.rkt"
         "../util/tests.rkt"
         "../util/log-controls.rkt"
         "../util/mutant-cmdline.rkt"
         "../util/shared-ctcs.rkt"
         "../configurations/config.rkt"
         "../configurations/configure-benchmark.rkt"
         "../configurables/configurables.rkt"
         "mutant-factory-data.rkt"
         "blame-trail-data.rkt"
         "integrity-metadata.rkt"
         (only-in "../orchestration/experiment-info.rkt" current-experiment-dir)
         process-queue/priority
         racket/file
         racket/format
         racket/match
         racket/system
         racket/set
         racket/port
         racket/file
         racket/logging
         racket/date
         racket/runtime-path
         racket/list
         syntax/parse/define
         (for-syntax racket/base))

(module+ test
  (provide (struct-out mutant)
           (struct-out revivals)
           (struct-out mutant-process)
           (struct-out dead-mutant-process)
           (struct-out bench-info)
           (struct-out factory)
           (struct-out blame-trail)
           process-limit
           data-output-dir
           factory-logger
           abort-on-failure?
           default-memory-limit/gb
           default-timeout/s
           MAX-FAILURE-REVIVALS
           MAX-TYPE-ERROR-REVIVALS
           sample-size
           copy-factory
           mutant-error-log
           test-mutant-flag
           current-result-cache

           run-all-mutants*config
           run-mutant*tests
           mutant-data-file-name
           spawn-mutant*test
           read-mutant-result
           process-outcome
           mutant->process-will
           abort-suppressed?

           #;
           make-cached-results-for
           #;
           make-progress-logger

           record/check-configuration-outcomes?
           record/check-configuration-outcome!
           setup-configuration-outcome-record/checking!))

(define debug:save-individual-mutant-outputs? #f)

(define MAX-CONFIG 'types)
(define MAX-FAILURE-REVIVALS 10)
(define MAX-TYPE-ERROR-REVIVALS 3)
(struct no-recorded-outcome () #:transparent)

(define (make-bench-config a-benchmark setting)
  (define mods (benchmark-untyped a-benchmark))
  (for/hash ([path (in-list mods)])
    (values (file-name-string-from-path path)
            setting)))

;; Outcomes in a blame trail that aren't one of these will go straight to the no-blame-handler
#;
(define/contract normal-blame-trail-outcomes
  (listof run-outcome/c)
  '(type-error runtime-error contract-violation))

(define process-limit (make-parameter 3))
(define data-output-dir (make-parameter "./mutant-data"))
(define abort-on-failure? (make-parameter #t))

(define current-result-cache (make-parameter (λ _ #f)))
#;(define current-progress-logger (make-parameter void))

(define current-sqlite-db-connection (make-parameter #f))
(define current-sqlite-db-table-name (make-parameter #f))

(define/contract record/check-configuration-outcomes?
  (parameter/c (or/c #f
                     (list/c 'record (mutant/c config/c run-outcome/c . -> . any))
                     (list/c 'check
                             (mutant/c config/c . -> . (or/c run-outcome/c no-recorded-outcome?)))
                     (list/c (or/c 'record 'check) path-string?)))
  (make-parameter #f))

(define-logger factory)
(define (log-factory-message level msg . vs)
  (when (log-level? factory-logger level)
    (log-message factory-logger
                 level
                 (apply format
                        (string-append "[~a] "
                                       (if (member level '(fatal error warning))
                                           (failure-msg level msg)
                                           msg))
                        (date->string (current-date) #t)
                        vs)
                 #f)))
(define-syntax-rule (log-factory level msg v ...)
  (log-factory-message 'level msg v ...))
(define (failure-msg failure-type m)
  (string-append "***** " (~a failure-type) " *****\n" m "\n**********"))

(define recvr (make-log-receiver factory-logger mutant-factory-log-level))

(when print-mutant-factory-logs?
  (void (thread (lambda ()
                  (let loop ()
                    (define v (sync recvr))
                    (printf "[~a] ~a~n" (vector-ref v 0) (vector-ref v 1))
                    (loop))))))

;; Main entry point of the factory
(define/contract (run-all-mutants*config bench
                                         config
                                         test-type
                                         ;; #:log-progress log-progress!
                                         ;; #:load-progress load-result-cache
                                         )
  (benchmark/c
   ; first  module-name?+natural? is the mutant
   ; second module-name?+natural? is the test
   ;; #:log-progress (module-name? natural? module-name? natural? path-to-existant-file? . -> . any)
   ;; #:load-progress
   ;; (-> (module-name? natural? module-name? natural? . -> . (or/c #f path-to-existant-file?)))
   . -> .
   boolean? ; experiment complete and sanity checks pass?
   )

  (parameterize (#;[current-progress-logger log-progress!]
                 #;[current-result-cache (load-result-cache)])
    (log-factory info @~a{Running on benchmark @bench})

    (define select-modules (configured:select-modules-to-mutate))
    (define mutatable-module-names (select-modules bench))
    (log-factory info "Benchmark has mutatable modules:~n~a" mutatable-module-names)

    (unless (directory-exists? (data-output-dir))
      (log-factory debug "Creating output directory ~a." (data-output-dir))
      (make-directory* (data-output-dir)))

    (define select-mutants (configured:select-mutants))
    (define starting-q
                      (make-process-queue
                       (process-limit)
                       (factory (bench-info bench config) (hash) (hash) 0)
                       < ;; lower priority value means schedule sooner (this was
                       ;; unconfigurable with the original implementation, now just
                       ;; stick to that original default)
                       #:kill-older-than
                       (if (current-run-with-condor-machines)
                           (*
                            2
                            60
                            60) ;; condor ought to run jobs pretty quick, so after 2h it's very likely stuck
                           (let-values ([{max-timeout _} (increased-limits bench)])
                             (+ max-timeout 30)))))

    (define starting-q-with-sanity
      (run-mutant*tests bench config test-type starting-q #f))

    (define process-q
      (for/fold ([process-q starting-q-with-sanity])
                ([module-to-mutate-name mutatable-module-names]
                 #:when #t
                 [mutation-index (select-mutants module-to-mutate-name bench)])
        (run-mutant*tests bench config test-type process-q (mutant #f module-to-mutate-name mutation-index))))


    (log-factory info "Finished enqueing all test mutants. Waiting...")
    (define process-q-finished (process-queue-wait process-q))
    (report-completion/sanity-checks bench select-mutants #;(load-result-cache))))

(define/contract (report-completion/sanity-checks bench select-mutants #;logged-results-for)
  (benchmark/c any/c
               (module-name? natural? natural? . -> . (or/c #f path-to-existant-file?))
               . -> .
               boolean? ; sanity checks pass?
               )

  (log-factory info "All mutants dead. Performing sanity checks...")
  #;(define mutatable-module-names (benchmark->mutatable-modules bench))
  #;(define testable-modules (benchmark->testable-modules bench))
  #;(define (something-recorded? module-to-mutate-name mutation-index test-mod test-id)
    (and (logged-results-for module-to-mutate-name mutation-index test-mod test-id) #t))
  #;(define something-logged-for-all-mutants*tests?
      (for*/and ([module-to-mutate-name mutatable-module-names]
                 [mutation-index (select-mutants module-to-mutate-name
                                                 bench)]
                 [module-to-mutate-name mutatable-module-names]
                 [mutation-index (select-mutants module-to-mutate-name bench)]
                 [test-mod testable-modules]
                 [test-id (get-all-test-ids test-mod bench)]
                 [all-trails-should-be-recorded? (in-value #t)])
        (define something-present?
          (something-recorded? module-to-mutate-name
                           mutation-index
                           test-mod
                           test-id))
        (unless something-present?
          (log-factory
           warning
           @~a{
               Expected something for @;
               @module-to-mutate-name @"@" @mutation-index, test: @test-mod @"@" @test-id to be recorded, but such a thing @;
               is @(if something-present? "recorded" "missing").
               }))
        something-present?))
    (define unexpected-state-encountered?
      (unbox abort-suppressed?))
    (define mutants-have-error-output?
      (and (file-exists? (mutant-error-log))
           ;; things like `echo '' > <log>` make it have size 1 or 2, and any real error messages will
           ;; have much more than 1 or 2
           (> (file-size (mutant-error-log)) 2)))
  (define (or-empty bool msg)
    (if bool msg ""))

  (define all-checks-pass?
    (and (not unexpected-state-encountered?)
         (not mutants-have-error-output?)))
  #;
  (log-factory-message
   (if all-checks-pass? 'info 'error)
   @~a{
    @(if all-checks-pass? "and basic sanity checks pass." "but with failing sanity checks.")
    @(or-empty (not something-logged-for-all-mutants*tests?)
              "⚠ Not all mutants have all of the expected blame trail samples.\n") @;
    @(or-empty unexpected-state-encountered? "⚠ Some unexpected states were encountered.\n") @;
    @(or-empty mutants-have-error-output? "⚠ Some mutants logged error messages.\n")})
  (log-factory-message
   (if all-checks-pass? 'info 'error)
   @~a{
       Experiment complete, @(if all-checks-pass?
                                 "and basic sanity checks pass."
                                 "but with failing sanity checks.")
       @(or-empty unexpected-state-encountered? "⚠ Some unexpected states were encountered.\n") @;
       @(or-empty mutants-have-error-output? "⚠ Some mutants logged error messages.\n")})
  all-checks-pass?)

;; Spawns a test mutant and if that mutant has a result at
;; max contract configuration, then samples the precision lattice
;; and spawns mutants for each samples point
;; Note that sampling the precision lattice is done indirectly by
;; just generating random configs
(define/contract (run-mutant*tests benchmark config test-type process-q mutant-program)
  (benchmark/c process-queue? #;(process-queue/c factory/c) (or/c mutant/c #f) . -> . (process-queue/c factory/c))

  (define module-to-mutate-name (and mutant-program (mutant-module mutant-program)))
  (define mutation-index        (and mutant-program (mutant-index mutant-program)))

  (log-factory info
               "  Trying to spawn mutant for ~a @ ~a."
               module-to-mutate-name
               mutation-index)
  (define bench (factory-bench (process-queue-get-data process-q)))
;  (define config (make-max-bench-config bench))

  (define testable-modules (benchmark->testable-modules benchmark))

  (for*/fold ([process-q process-q])
                  ([test-mod testable-modules]
                   [test-id (get-all-test-ids test-type test-mod benchmark)])
    (log-factory debug
                  "  Trying to spawn test for [~a] ~a @ ~a."
                  test-type
                  test-mod
                  test-id)

    #;(define result-cache-has-something?
       (and ((current-result-cache) module-to-mutate-name
                                    mutation-index
                                    test-mod
                                    test-id)
            #t))

  (define (will:do-nothing current-process-q dead-proc)
            current-process-q)
  (cond
   #;[result-cache-has-something? process-q]
   [else
     (log-factory
      info
      @~a{
          Spawning test mutant for @;
          [@test-type] @;
          @module-to-mutate-name @"@" @mutation-index @;
          @test-mod @"@" @test-id @;
          because nothing found in cache
          })
     (spawn-mutant*test process-q
                   (or module-to-mutate-name test-mod)
                   (or mutation-index 0)
                   test-type
                   test-mod
                   test-id
                   config
                   will:do-nothing
                   #:test-mutant? #t
                   #:fake-mutation? (not mutant-program))])))

(define (increased-limits bench)
  (values (* 2 (default-timeout/s))
          (* 2 (default-memory-limit/gb))))

(define/contract (spawn-mutant*test process-q
                                    module-to-mutate-name
                                    mutation-index
                                    test-type
                                    test-mod
                                    test-id
                                    precision-config
                                    mutant-will
                                    [revival-counts (revivals 0 0)]
                                    #:fake-mutation? fake-mutation?
                                    #:timeout/s [timeout/s #f]
                                    #:memory/gb [memory/gb #f]
                                    #:following-trail [trail-being-followed #f]
                                    #:test-mutant? [test-mutant? #f])
  (->i ([process-q              (process-queue/c factory/c)]
        [module-to-mutate-name  module-name?]
        [mutation-index         natural?]
        [test-type test-type/c]
        [test-mod    module-name?]
        [test-id                natural?]
        [precision-config       config/c]
        [mutant-will            mutant-will/c]
        #:fake-mutation? [fake-mutation? boolean?])
       ([revival-counts revivals/c]
        #:following-trail [trail-being-followed  (or/c #f blame-trail/c)]
        #:test-mutant?    [test-mutant?          boolean?]
        #:fake-mutation?  [fake-mutation?        boolean?]
        #:timeout/s       [t/s                   (or/c #f number?)]
        #:memory/gb       [m/gb                  (or/c #f number?)])
       #:pre/desc {trail-being-followed test-mutant?}
       (or (= 1 (count (disjoin unsupplied-arg? false?)
                       (list trail-being-followed test-mutant?)))
           @~a{
               Exactly one of @;
               #:following-trail (@trail-being-followed) @;
               and #:test-mutant? (@test-mutant?) @;
               must be specified.
               })
       [result (process-queue/c factory/c)])

  (define current-factory (process-queue-get-data process-q))
  (match-define (factory (bench-info the-benchmark _)
                         _
                         _
                         mutants-spawned)
    current-factory)
  (define outfile (build-path (data-output-dir)
                              (format "~a_m~a_~a_~a_t~a_~a.rktd"
                                      module-to-mutate-name
                                      mutation-index
                                      test-type
                                      test-mod
                                      test-id
                                      mutants-spawned)))
  (define the-benchmark-configuration
       (configure-benchmark the-benchmark
                            precision-config #:test-mod test-mod))

  (define mutant-id mutants-spawned)
  (define (spawn-the-mutant)
    (define mutant-ctl
      (spawn-mutant-runner the-benchmark-configuration
                           module-to-mutate-name
                           mutation-index
                           outfile
                           (current-configuration-path)
                           #:timeout/s timeout/s
                           #:memory/gb memory/gb
                           #:test-type test-type
                           #:test-id test-id
                           #:fake-mutation? fake-mutation?
                           #:save-output (and debug:save-individual-mutant-outputs?
                                              (build-path (data-output-dir)
                                                          (format "~a.rktd"
                                                                  mutant-id)))))
    ;; record the args that the script will be run with (this is just to save to the database)
    (define recorded-args
      (make-mutant-runner-script-args the-benchmark-configuration
                                      module-to-mutate-name
                                      mutation-index
                                      (current-configuration-path)
                                      (current-experiment-dir)
                                      #:fake-mutation? fake-mutation?
                                      #:log-mutation-info? (current-mutant-runner-log-mutation-info?)
                                      #:test-type test-type
                                      #:test-id test-id
                                      #:timeout/s (or timeout/s (default-timeout/s))
                                      #:memory/gb (or memory/gb (default-memory-limit/gb))
                                      #:save-output (and debug:save-individual-mutant-outputs?
                                                         (build-path (data-output-dir)
                                                                     (format "~a.rktd"
                                                                             mutant-id)))))
    (define mutant*test-proc
      (mutant*test-process (mutant #f module-to-mutate-name mutation-index)
                           precision-config
                           outfile
                           mutant-id
                           (blame-trail 'test '())
                           revival-counts
                           ;; coerce to bool
                           (and (or timeout/s memory/gb) #t)
                           recorded-args
                           test-type
                           test-mod
                           test-id
                           fake-mutation?))
    (log-factory
     info
     "    Spawned mutant runner with id [~a] for ~a @ ~a, testing [~a] ~a @ ~a > ~a."
     mutant-id
     module-to-mutate-name
     mutation-index
     test-type
     test-mod
     test-id
     (pretty-path outfile))
    (process-info mutant*test-proc
                  mutant-ctl
                  (mutant->process-will mutant-will)))
  (log-factory
   debug
   @~a{    Mutant [@mutant-id] has config @~v[(serialize-config precision-config)]})

  ;; lower priority means schedule sooner
  (define this-mutant-priority 1)
  (process-queue-enqueue
   (process-queue-set-data process-q
                       (copy-factory current-factory
                                     [total-mutants-spawned
                                      (add1 mutants-spawned)]))
   spawn-the-mutant
   this-mutant-priority
   #:defer-update? #t))

;; There is some common housekeeping that must be performed in every mutant
;; will, regardless of what kind of mutant or the details of its particular will.
;; In particular:
;; - Respawning the mutant a limited number of times if the mutant fails
;; - Otherwise, converting the mutant-process into a dead-mutant-process
;; - Logging the result of the mutant (and updating the factory with it)
(define/contract (mutant->process-will mutant-will)
  (mutant-will/c . -> . process-will/c)

  (define (outer-will process-q the-process-info)
    (match-define (and mutant*test-proc
                       (mutant*test-process (mutant #f mutant-mod mutant-id)
                                            config
                                            file
                                            id the-blame-trail
                                            revival-counts
                                            increased-limits?
                                            recorded-args
                                            test-type
                                            test-mod
                                            test-id
                                            fake-mutation?))
      (process-info-data the-process-info))
    ;; Read the result of the mutant before possible consolidation
    (define status ((process-info-ctl the-process-info) 'status))
    (define maybe-result
      ;; ll: Check before reading to reduce the number of warnings
      ;; emitted for an errored mutant, otherwise would warn wrong
      ;; output as well as error
      (if (equal? status 'done-ok)
          (read-mutant-result mutant*test-proc)
          (file->string (mutant-process-file mutant*test-proc))))
    (match (cons status maybe-result)
      [(or (cons 'done-error _)
           (cons 'done-ok (? eof-object?)))
       (maybe-revive-failed-mutant process-q
                                   mutant*test-proc
                                   status
                                   maybe-result
                                   mutant-will)]
      [(cons 'done-ok (? run-status? result))
       ((configured:add-table-entry!) (current-sqlite-db-table-name) (current-sqlite-db-connection)
                                      #:configuration config
                                      #:test-type test-type
                                      #:module-under-test test-mod
                                      #:test-index test-id
                                      #:mutant-module (if fake-mutation? "NO_MUTATIONS" mutant-mod)
                                      #:mutation-index mutant-id
                                      #:run-status result
                                      #:cmd-line-args (string-join (cons "racket" recorded-args)))
       (log-factory info
                    @~a{
                        Sweeping up dead mutant [@id]: @mutant-mod @"@" @mutant-id, @;
                        result: @;
                        @(match result
                           [(struct* run-status
                                     ([outcome (and outcome
                                                    (or 'contract-violation
                                                        'type-error
                                                        'runtime-error))]
                                      [blamed (list (? string? mod-names) ...)]))
                            (~a outcome " blaming " mod-names)]
                           [(struct* run-status
                                     ([outcome (and outcome
                                                    'runtime-error)]
                                      [blamed #f]))
                            "runtime-error without inferred blame"]
                           [(struct* run-status
                                     ([outcome o]))
                            o]), @;
                        config: @~s[(serialize-config config)]
                        })
       (match (record/check-configuration-outcome! mutant*test-proc result)
         [#t
         (log-factory fatal
          (format "Revive-type-error case encountered: mutant*test-proc = ~a and result = ~a Don't know what to do about it." mutant*test-proc result))
          #;(revive-type-error-mutant process-q
                                    mutant-proc
                                    status
                                    maybe-result
                                    mutant-will)]
         [#f
          (define dead-mutant-proc
            (dead-mutant-process (mutant #f mutant-mod mutant-id)
                                 config
                                 result
                                 id
                                 the-blame-trail
                                 increased-limits?))
          (delete-file file)
          (mutant-will process-q dead-mutant-proc)])]))
  outer-will)

(define/contract (maybe-revive-failed-mutant process-q
                                             a-mutant-process
                                             status
                                             maybe-result
                                             mutant-will)
  (->i ([process-q         (process-queue/c factory/c)]
        [a-mutant-process  mutant-process/c]
        [status            (or/c 'done-ok 'done-error)]
        [maybe-result      {status}
                           (match status
                             ['done-ok eof-object?]
                             ['done-error any/c])]
        [mutant-will       mutant-will/c])
       [result (process-queue/c factory/c)])

  (match-define (struct* mutant-process
                         ([id             id]
                          [blame-trail    the-blame-trail]
                          [mutant         (mutant #f mod index)]
                          [config         config]
                          [revival-counts (revivals for-failure
                                                    for-type-error)]))
    a-mutant-process)

  (match-define (struct* mutant*test-process
                       ([test-type test-type]
                        [test-mod test-mod]
                        [test-id test-id]
                        [fake-mutation? fake-mutation?]))
    a-mutant-process)

  (cond [(>= for-failure MAX-FAILURE-REVIVALS)
         (log-factory error
                      "Runner errored all ~a / ~a tries on mutant:
 [~a] ~a @ ~a (test [~a] ~a ~a) with config
~v"
                      for-failure MAX-FAILURE-REVIVALS
                      id mod index
                      test-type test-mod test-id
                      (serialize-config config))
         (maybe-abort "Revival failed to resolve mutant errors"
                      process-q)]
        [else
         (log-factory warning
                      "Runner errored on mutant [~a] ~a @ ~a (test [~a] ~a ~a) with config
~v

Exited with ~a and produced result: ~v

Attempting revival ~a / ~a
"
                      id mod index
                      test-type test-mod test-id
                      (serialize-config config)
                      status maybe-result
                      (add1 for-failure) MAX-FAILURE-REVIVALS)
         (spawn-mutant*test process-q
                       mod
                       index
                       test-type
                       test-mod
                       test-id
                       config
                       mutant-will
                       (revivals (add1 for-failure)
                                 for-type-error)
                       #:fake-mutation? fake-mutation?
                       #:following-trail (match the-blame-trail
                                           [(? blame-trail? bt) bt]
                                           [else                #f])
                       #:test-mutant? (equal? the-blame-trail
                                              test-mutant-flag))]))

(define (mutant-data-file-name mod-name mutation-index)
  @~a{
      @|mod-name|@;
      _@;
      @|mutation-index|@;
      .rktd
      })

#;
(define/contract (record-blame-trail! the-factory the-blame-trail)
  (factory/c
   (and/c blame-trail/c
          (flat-named-contract
           'blame-trail-of-length-at-least-one
           (match-lambda [(blame-trail _ (not '())) #t]
                         [else #f])))
   . -> .
   factory/c)

  (match-define (and the-mutant
                     (mutant #f module-to-mutate-name mutation-index))
    (dead-mutant-process-mutant (first (blame-trail-parts the-blame-trail))))
  (define the-results (factory-results the-factory))
  (define mutant-data-file
    (match (hash-ref the-results the-mutant #f)
      [#f (build-path (data-output-dir)
                      (mutant-data-file-name module-to-mutate-name
                                             mutation-index))]
      [path path]))
  (append-blame-trail-to-mutant-data! mutant-data-file
                                      the-blame-trail)
  ((current-progress-logger) module-to-mutate-name
                             mutation-index
                             (blame-trail-id the-blame-trail)

                             mutant-data-file)
  (copy-factory the-factory
                [results (hash-set the-results the-mutant mutant-data-file)]))

#;
(define (append-blame-trail-to-mutant-data! mutant-data-file
                                            the-blame-trail)
  (define summary (summarize-blame-trail the-blame-trail))
  (with-output-to-file mutant-data-file
    #:mode 'text
    #:exists 'append
    (λ _ (writeln summary))))

#;
(define (summarize-blame-trail the-blame-trail)
  (define mutant-summaries
    (map summarize-dead-mutant-process
         (blame-trail-parts the-blame-trail)))
  (match-define (list* (struct* dead-mutant-process
                                ([mutant (mutant #f mod index)]))
                       _)
    (blame-trail-parts the-blame-trail))
  (blame-trail-summary mod
                       index
                       (blame-trail-id the-blame-trail)
                       mutant-summaries))

(define summarize-dead-mutant-process
  (match-lambda [(struct* dead-mutant-process
                          ([id id]
                           [result result]
                           [config config]))
                 (mutant-summary id
                                 result
                                 (serialize-config config))]))


(define/contract (read-mutant-result mutant-proc)
  (mutant-process/c . -> . (or/c run-status? eof-object?))

  (define path (mutant-process-file mutant-proc))
  (define (report-malformed-output . _)
    (match-define (mutant-process (mutant _ mod index) config _ id _ _ _ _)
      mutant-proc)
    (log-factory warning
                 "Result read from mutant output not of the expected shape.
Expected: a run-status with a valid pair of outcome/blamed
Found: ~v
If this has the right shape, it may contain an unreadable value.

Mutant: [~a] ~a @ ~a with config:
~v
"
                 (file->string path)
                 id mod index
                 (serialize-config config))
    eof)
  (if (file-exists? path)
      (with-handlers ([exn:fail:read? report-malformed-output])
        (match (with-input-from-file path read)
          [(and (or (struct* run-status
                             ([outcome (or 'completed
                                           'syntax-error
                                           'timeout
                                           'skipped
                                           'index-exceeded
                                           'oom)]
                              [blamed #f]
                              [errortrace-text #f]
                              [errortrace-stack #f]
                              [context-stack #f]))
                    (struct* run-status
                             ([outcome 'type-error]
                              [blamed (not #f)]
                              [errortrace-text #f]
                              [errortrace-stack #f]
                              [context-stack #f]))
                    (struct* run-status
                             ([outcome (or 'contract-violation
                                           'runtime-error)]
                              [blamed (not #f)]
                              [errortrace-text (not #f)]
                              [errortrace-stack (? list?)]
                              [context-stack (? list?)]))
                    (struct* run-status
                             ([outcome (or 'runtime-error
                                           'test-failure)]
                              [blamed #f]
                              [errortrace-text (not #f)]
                              [errortrace-stack (? list?)]
                              [context-stack (? list?)])))
                result/well-formed)
           result/well-formed]
          [else (report-malformed-output)]))
      eof))

;; dead-mutant-process? -> run-outcome/c
(define (process-outcome dead-proc)
  (run-status-outcome (dead-mutant-process-result dead-proc)))

(define abort-suppressed? (box #f))
(define (maybe-abort reason continue-val #:force [force? #f])
  ;; Mark the mutant error file before it gets garbled with error
  ;; messages from killing the current active mutants
  (log-factory error "Received abort signal with reason: ~a" reason)
  (cond [(or (abort-on-failure?) force?)
         (call-with-output-file (mutant-error-log)
           #:exists 'append #:mode 'text
           (λ (out)
             (fprintf out
                      "
~n~n~n~n~n~n~n~n~n~n
================================================================================
                              Factory aborting
  Reason: ~a
================================================================================
~n~n~n~n~n~n~n~n~n~n
"
                      reason)))
         (exit 1)
         ;; Return continue-val in case of custom exit handler.
         ;; This should probably only matter for testing, so that the
         ;; contracts of various functions don't blow up.
         continue-val]
        [else
         (log-factory warning
                      "Continuing execution despite abort signal...")
         (set-box! abort-suppressed? #t)
         continue-val]))

(define (pretty-path p)
  (path->string (find-relative-path (simple-form-path (current-directory))
                                    (simple-form-path p))))

;; mutant-process? run-status? -> boolean?
;;
;; Checks the outcome of the given configuration, possibly recording or
;; reporting an error about it, and returns whether to retry the process or
;; continue with executing its will.
;;
;; A config needs to be retried a few times if it produces a type-error in order
;; to verify that it really produces a type-error, thanks to a hard-to-pin-down
;; bug in TR that very occaisonally causes some programs to raise "duplicate
;; annotation" type errors for no apparent reason whatsoever. We don't know what
;; causes it or how to fix it, so we need to work around it by checking a few
;; times that any type error really is a type-error.
(define (record/check-configuration-outcome! mutant-proc result)
  (match-define (struct* mutant-process ([mutant mutant]
                                         [config config]
                                         [revival-counts (revivals _ for-type-error)]))
    mutant-proc)
  (define (get-outcome outcome-for)
    (outcome-for mutant config))
  (define retry #t)
  (define continue #f)
  (match* {result (record/check-configuration-outcomes?)}
    [{(struct* run-status ([outcome 'type-error]))
      `(record ,record!)}
     #:when (< for-type-error MAX-TYPE-ERROR-REVIVALS)
     retry]
    [{(struct* run-status ([outcome outcome]))
      `(record ,record!)}
     (record! mutant config outcome)
     continue]

    [{(struct* run-status ([outcome 'type-error]))
      `(check ,(app get-outcome (not 'type-error)))}
     #:when (< for-type-error MAX-TYPE-ERROR-REVIVALS)
     retry]
    [{(struct* run-status ([outcome (and real-outcome (not (or 'timeout 'oom)))]))
      `(check ,(app get-outcome recorded-outcome))}
     #:when (not (outcome-compatible-with? recorded-outcome real-outcome))
     (maybe-abort
      @~a{
          Found that a configuration produces @real-outcome, but configuration outcomes db @;
          says that it should produce something compatible with @recorded-outcome
          Mutant: @mutant @(serialize-config config)
          }
      continue)]

    [{_ _}
     (match (record/check-configuration-outcomes?)
       [`(check ,checker)
        (log-factory debug @~a{outcome check: @result looks ok! checker says it should be: @(get-outcome checker)})]
       [else (void)])
     continue]))

(define (outcome-compatible-with? recorded actual)
  (match (list recorded actual)
    [(list (or (== (no-recorded-outcome)) 'timeout 'oom) _) #t]
    [(list-no-order 'syntax-error _)                        #f]
    [(list 'type-error 'type-error)                         #t]
    [(list-no-order 'type-error (not 'type-error ))         #f]
    [else
     ;; all other modes should be 'weaker' than TR's recorded result
     (define ordering '(contract-violation runtime-error completed))
     (unless (and (index-of ordering actual)
                  (index-of ordering recorded))
       (log-factory error
                    @~a{Outcome checking: unrecognized outcome in @recorded or @actual ?}))
     (>= (index-of ordering actual)
         (index-of ordering recorded))]))

#;
(define progress-log (make-parameter #f))
#;
(define (make-cached-results-for progress-info-hash)
  (λ (module-to-mutate-name
      mutation-index
      test-mod
      test-id)
    (hash-ref progress-info-hash
              (list module-to-mutate-name
                    mutation-index
                    test-mod
                    test-id)
              #f)))
#;
(define (make-progress-logger log-progress!/raw)
  (λ (module-to-mutate-name
      mutation-index
      test-mod
      test-id

      data-file)
    (log-progress!/raw (cons (list module-to-mutate-name
                                   mutation-index
                                   test-mod
                                   test-id)
                             ;; ensure it's an absolute path in case we resume
                             ;; from another directory
                             (path->string
                              (simple-form-path data-file))))))

(define (setup-configuration-outcome-record/checking!)
  (match (record/check-configuration-outcomes?)
    [`(record ,path)
     (make-parent-directory* path)
     (define-values {log-outcome!/raw finalize-log!}
       (initialize-progress-log! path
                                 #:exists 'append))
     (define (log-outcome! mutant config outcome)
       (log-outcome!/raw (cons (list mutant (serialize-config config)) outcome)))
     (record/check-configuration-outcomes? `(record ,log-outcome!))
     finalize-log!]
    [`(check ,path)
     (define outcomes (make-immutable-hash (file->list path)))
     (define (outcome-for mutant config)
       (hash-ref outcomes
                 (list mutant (serialize-config config))
                 (thunk
                  (log-factory info
                               @~a{
                                   Outcome checking: no outcome found in log for @;
                                   @mutant @(serialize-config config)
                                   })
                  (no-recorded-outcome))))
     (record/check-configuration-outcomes? `(check ,outcome-for))
     void]
    [else void]))

(module+ main
  (require racket/cmdline)
  (define bench-path-to-run (make-parameter #f))
  (define contract-setting (make-parameter #f))
  (define metadata-file (make-parameter #f))
  (define configuration-path (make-parameter #f))
  (define test-type (make-parameter #f))
  (command-line
   #:once-each
   [("-x" "--experiment-dir")
    dir
    "Directory for this experiment. Mandatory."
    (current-experiment-dir dir)]
   [("-b" "--benchmark")
    path
    "Path to benchmark to run. Mandatory."
    (bench-path-to-run path)]
   [("-d" "--test-type")
    specified-test-type
    "Test type (e.g. 'rand' for tests in a directory 'rand-tests'). Mandatory"
    (test-type specified-test-type)]
   [("-t" "--contract-setting")
    ctc-setting
    "Contract setting (max, types, or none). Mandatory."
    (contract-setting (string->symbol ctc-setting))]
   [("-c" "--config")
    path
    "Path to the configuration to use. Mandatory."
    (configuration-path path)]
   [("-o" "--output-dir")
    dir
    "Data output directory. Mandatory."
    (data-output-dir dir)]
   [("-n" "--process-limit")
    n
    ("Number of processes to have running at once."
     @~a{Default: @(process-limit)})
    (process-limit (string->number n))]
   [("-e" "--error-log")
    path
    "File to which to append mutant errors. Default: ./mutant-errors.txt"
    (mutant-error-log path)]
   [("-k" "--keep-going")
    "Continue despite encountering failure conditions. Default: #f"
    (abort-on-failure? #f)]
   [("-s" "--sample-size")
    n
    "Number of blame trail roots to sample. Default: 96"
    (sample-size (string->number n))]
   ;; TODO resume from point reached in database instead of progress log, which 
   ;; recorded blame trails
   #;
   [("-l" "--progress-log")
    path
    ("Record progress in the given log file."
     "If it exists and is not empty, resume from the point reached in the log."
     "Mandatory.")
    (progress-log path)]
   [("-m" "--metadata")
    path
    ("Record metadata about the configuration used to run an experiment in the given file."
     "This is useful information, and it is used to prevent resuming an experiment with a"
     "different configuration than it was started with.")
    (metadata-file path)]
   [("-P" "--record-configuration-outcomes") ; p for parity
    log-path
    ("For every configuration visited, record in the given db (which may not exist yet) the"
     "configuration's outcome."
     "Upon completion, the db can be used with `-p` (which see).")
    (record/check-configuration-outcomes? `(record ,log-path))]
   [("-p" "--check-configuration-outcomes")
    log-path
    ("Check the outcome of every configuration visited against those recorded in the given db"
     "(produced by `-P`). If the outcome of a configuration does not match the outcome recorded"
     "in the db, signal a fatal error.")
    (record/check-configuration-outcomes? `(check ,log-path))])

  (unless (bench-path-to-run)
    (raise-user-error 'mutant-factory "Error: must provide benchmark to run."))
  (unless (configuration-path)
    (raise-user-error 'mutant-factory "Error: must provide a configuration."))
  (unless (data-output-dir)
    (raise-user-error 'mutant-factory "Error: must provide a data output dir."))
  #;
  (unless (progress-log)
    (raise-user-error 'mutant-factory "Error: must provide a progress-log."))

  (install-configuration! (configuration-path))

  (define bench-to-run (read-benchmark (bench-path-to-run)))

  #;
  (when (and (directory-exists? (data-output-dir))
             (not (progress-log)))
    (eprintf "Output directory ~a already exists; remove? (y/n): "
             (data-output-dir))
    (match (read)
      [(or 'y 'yes) (delete-directory/files (data-output-dir))]
      [_ (eprintf "Not deleted.~n")]))

  (when (metadata-file)
    (define info
      (metadata-info (metadata-file)
                     (bench-path-to-run)
                     (configuration-path)
                     (record/check-configuration-outcomes?)))
    (unless (create/check-metadata-integrity! info)
      (raise-user-error
       'mutant-factory
       @~a{
           This appears to be a resumption of the experiment, but the metadata recorded at @;
           @(metadata-file) for the previous run does not match that of this run's configuration.
           Recorded info: @(~s (file->value (metadata-file)))
           This run info: @(~s (metadata-info->id info))
           Aborting.
           })))

  (define finalize-configuration-outcomes!
    (setup-configuration-outcome-record/checking!))

  #;(define (make-cached-results-function)
    (define progress-info-hash
      (match (progress-log)
        [(? file-exists? path) (make-immutable-hash (file->list path))]
        [else (hash)]))
    (make-cached-results-for progress-info-hash))
  #;(define-values {log-progress!/raw finalize-log!}
    (initialize-progress-log! (progress-log)
                              #:exists 'append))

  (log-factory info
               @~a{
                   Running experiment with config @;
                   @(configuration-path) and contract level @;
                   @(contract-setting) and test type @;
                   @(test-type)
                   })

  ;; Create the sqlite database and the table for this benchmark
  ((configured:ensure-db!))
  (define conn ((configured:connect-to-db)))
  (define db-table-name (benchmark->name bench-to-run))
  ((configured:ensure-table!) db-table-name conn)

  (define completed+checks-pass?
    (parameterize ([date-display-format 'iso-8601]
                   [current-sqlite-db-connection conn]
                   [current-sqlite-db-table-name db-table-name])
      (define config (make-bench-config bench-to-run (contract-setting)))
      (run-all-mutants*config bench-to-run
                              config
                              (test-type)
                              ;; #:log-progress (make-progress-logger log-progress!/raw)
                              ;; #:load-progress make-cached-results-function
                              )))

  #;(finalize-log!)
  (finalize-configuration-outcomes!)

  (exit 0))
