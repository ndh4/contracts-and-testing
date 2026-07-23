#lang at-exp rscript

(provide update-host!
         setup-dbs!
         check-host-empty!
         launch-benchmarks!
         wait-for-current-jobs-to-finish
         #;download-results!
         format-status
         summarize-experiment-status
         prompt-for-cleanup!
         help!:continue?
         notify-phone!

         zythos
         zythos-ssh/one-job-per-mutant
         zythos-local/one-job-per-mode
         zythos-local/one-job-per-mutant/batched
         benbox
         local
         nathaniel)

(require syntax/parse/define 
         racket/date
         "condor-host.rkt"
         "direct-access-host.rkt"
         "../util/option.rkt" 
         "experiment-info.rkt") 

(define-runtime-paths
  [store-path "../../../experiment-data/experiment-manager"] 
  [std-experiment-runner-template "./standard-experiment-runner-script-template.sh"] 
  [experiment-info.rkt "experiment-info.rkt"] 
  [project-path "../../.."]) 

(define (local-version-mixin c)
  (class c
    (super-new)
    (define/override (system/host #:interactive? [interactive? #f] . parts)
      (define cmd-str (apply ~a (add-between parts " ")))
      (system cmd-str))
    (define/override (system/host/string #:interactive? [interactive? #f] . parts)
      (define out-str (open-output-string))
      (parameterize ([current-output-port out-str]
                     [current-error-port out-str])
        (system/host #:interactive? interactive? . parts))
      (get-output-string out-str))
    (define/override (scp #:from-host [from-host-path #f]
                          #:to-local [to-local-path #f]
                          #:from-local [from-local-path #f]
                          #:to-host [to-host-path #f])
      (match (list from-host-path
                   to-local-path
                   from-local-path
                   to-host-path)
        [(or (list from to #f #f)
             (list #f #f from to))
         (system/exit-code @~a{cp -r '@from' '@to'})]
        [else (raise-user-error 'scp "Bad argument combination")]))))

(define local-direct-host% (local-version-mixin direct-access-host%))
(define local-condor-host% (local-version-mixin condor-host%))



;; host<%> -> (option/c results?)
(define (get-results a-host)
  (define outpath (get-field host-output-path a-host))
  (cond
    [(directory-exists? outpath)
     (define info-str
       (send a-host
             system/host/string
             (get-field host-racket-path a-host)
             (build-path (get-field host-utilities-path a-host) "check-experiment-results.rkt")
             "-w"
             (get-field host-output-path a-host)))
     (match info-str
       [(regexp "^#hash")
        (cond
         [(string-contains? info-str "?")
          (eprintf @~a{Unable to get experiment results summary for host @a-host, @;
                       found: @~v[info-str]})
          absent]
         [else (call-with-input-string info-str read)])]
       [else
        (eprintf @~a{
                     Unable to get experiment results summary for host @a-host, @;
                     found: @~v[info-str]

                     })
        absent])]
    [else
     (eprintf @~a{
                  Unable to get experiment results summary for host @a-host, @;
                  results directory @outpath does not yet exist

                  })
     absent]))

(define (try-infer-benchmark-from-data-name benchmark-data-name)
  (define ((prefix-or-suffix-of str) maybe-pre-or-suffix)
    (or (string-prefix? str maybe-pre-or-suffix)
        (string-suffix? str maybe-pre-or-suffix)))
  (findf (prefix-or-suffix-of benchmark-data-name)
         experiment-benchmarks))

;; host? (listof (cons/c symbol? symbol?)) -> (option/c (listof (option/c (and/c real? (between/c 0 1)))))
(define (get-progress a-host benchmarks+ctc-levels)
  #;(define benchmark
    (if (member benchmark experiment-benchmarks)
        benchmark
        (try-infer-benchmark-from-data-name benchmark)))
  (cond [(empty? benchmarks+ctc-levels)
         empty]
        [else
         (define benchmark+level-paths (for/list ([benchmark+ctc-level (in-list benchmarks+ctc-levels)])
                                   (define benchmark (car benchmark+ctc-level))
                                   (define ctc-level (cdr benchmark+ctc-level))
                                   (build-path (get-field host-output-path a-host) benchmark ctc-level)))
         (define progress-str
           (send a-host
                 system/host/string
                 (get-field host-racket-path a-host)
                 (build-path (get-field host-utilities-path a-host) "check-experiment-progress.rkt")
                 "-r"
                 .
                 benchmark+level-paths))
         (define progresses (string->value progress-str))
         (if (list? progresses)
             (for/list ([% (in-list progresses)])
               (match %
                 [(? number? n) n]
                 [else absent]))
             absent)]))

;; summary/c :=
;; (hash 'completed                         (listof (list/c string? string? string?))
;;       (or/c 'incomplete 'errored 'other) (listof (list/c string? string? string? (option/c real?))))

;; host<%> -> (option/c summary/c)
(define (summarize-experiment-status a-host)
  (define (add-progress incomplete-benchs)
    (match-define (list (list names _ ctc-levels) ...) incomplete-benchs)
    (define progresses (get-progress a-host (map cons names ctc-levels)))
    (for/list ([job-id (in-list incomplete-benchs)]
               [progress (in-list (if (absent? progresses)
                                      (make-list (length incomplete-benchs) absent)
                                      progresses))])
      (append job-id (list progress))))
  (option-let*
   ([results (get-results a-host)])
   (for/fold ([results+progress results])
             ([result-kind (in-list '(incomplete errored other))])
     (hash-update results+progress
                  result-kind
                  add-progress))))

(define (stuck-jobs active-jobs maybe-summary)
  (for*/list ([summary (in-option maybe-summary)]
              [incomplete-jobs (in-value (append (hash-ref summary 'incomplete)
                                                 ;; lltodo: this is to handle a
                                                 ;; bug in
                                                 ;; check-experiment-progress.rkt
                                                 (hash-ref summary 'errored)))]
              [maybe-benchmark (in-list active-jobs)]
              [benchmark (in-option maybe-benchmark)]
              [incomplete-job (in-list incomplete-jobs)]
              #:when (match incomplete-job
                       [(list (== (first benchmark))
                              (== (second benchmark))
                              (? (>/c 0.97)))
                        #t]
                       [else #f]))
    benchmark))

(define (restart-job! a-host job-info)
  (match-define (list benchmark config contract-setting) job-info)
  (option-let* ([_ (send a-host cancel-job! benchmark config #:contract-setting contract-setting)]
                [_ (send a-host submit-job! benchmark config #:contract-setting contract-setting)])
               (void)))

(define job-restart-history (make-hash))
(define (restart-stuck-jobs! a-host
                             [maybe-jobs (send a-host get-jobs 'both)]
                             [maybe-summary (summarize-experiment-status a-host)])
  (define (should-restart? job-info)
    (match-define (list restart-count skips-so-far)
      (hash-ref job-restart-history
                job-info
                (list 0 0)))
    (= restart-count skips-so-far))
  (define (restart+record! job-info)
    (displayln @~a{@(date->string (current-date) #t) Restarting stuck job: @job-info})
    (restart-job! a-host job-info)
    (hash-update! job-restart-history
                  job-info
                  (match-lambda [(list restart-count _) (list (add1 restart-count) 0)])
                  (list 0 0)))
  (define (record-skipped-start! job-info)
    (hash-update! job-restart-history
                  job-info
                  (match-lambda [(list restart-count skips) (list restart-count (add1 skips))])
                  (list 0 0)))

  (for* ([summary (in-option maybe-summary)]
         [jobs (in-option maybe-jobs)]
         [job-info (in-list (stuck-jobs (first jobs) summary))])
    (if (should-restart? job-info)
        (restart+record! job-info)
        (record-skipped-start! job-info))))

(define (summary-empty? summary)
  (match summary
    [(hash-table [_ '()] ...) #t]
    [else #f]))
(define (summary-has-errors? summary)
  (match summary
    [(hash-table ['completed _]
                 ['incomplete _]
                 [(or 'errored 'other) '()] ...)
     #f]
    [else #t]))
(define (missing-completed-benchmarks summary expected-benchmarks)
  (define completed (hash-ref summary 'completed))
  (define completed-benchmarks (map first completed))
  (set-subtract expected-benchmarks completed-benchmarks))
(define (config/mode-complete? summary expected-benchmarks)
  (define completed (hash-ref summary 'completed))
  (match completed
    [(list (list _ config-names) ..1)
     (and (= (set-count (apply set config-names)) 1)
          (empty? (missing-completed-benchmarks summary expected-benchmarks)))]
    [else #f]))

;; host?
;; [(host? (list/c (listof jobinfo?) x2) summary? -> any)]
;; ->
;; (or/c 'complete 'empty 'error)
(define (wait-for-current-jobs-to-finish host
                                         [periodic-action! void]
                                         #:period [sleep-period 15]
                                         #:print? [print? #f]
                                         #:expected-benchmarks [expected-benchmarks experiment-benchmarks])
  (when print?
    (displayln @~a{Waiting for current jobs to finish on @host ...}))
  (let loop ()
    (define current-status
      (option-let*
       ([summary (summarize-experiment-status host)]
        [jobs (send host get-jobs 'both)])

       (match-define (list active-jobs pending-jobs) jobs)
       (periodic-action! host jobs summary)
       (define no-jobs? (and (empty? active-jobs) (empty? pending-jobs)))
       (cond [(and no-jobs? (config/mode-complete? summary expected-benchmarks))
              'complete]
             [(and no-jobs? (summary-empty? summary))
              'empty]
             [(and no-jobs? (summary-has-errors? summary))
              (if (help!:continue? @~a{@host has errors}
                                   @~a{
                                       Found errors on @host, summary:
                                       @(format-status host summary)
                                       Resume waiting for finish? (No means abort): 
                                       })
                  (loop)
                  'error)]
             [no-jobs?
              (if (help!:continue? @~a{@host lost jobs}
                                   @~a{
                                       @host seems to have lost jobs. @;
                                       Expected to find jobs for all of @expected-benchmarks;
                                       Summary:
                                       @(format-status host summary)
                                       Re-run the missing jobs manually, and then continue.
                                       Resume waiting for finish? (No means abort): 
                                       })
                  (loop)
                  'error)]
             [else
              (when print?
                (printf "~a Sleeping for ~a min~nCurrent status:~n~a~n"
                        (date->string (current-date) #t)
                        sleep-period
                        (format-status host summary jobs)))
              (sleep (* sleep-period 60))
              (loop)])))
    (match current-status
      ['continue (loop)]
      [(? absent?)
       (displayln
        @~a{
            Unable to get summary or jobs, just continuing to wait
            })
       (sleep (* sleep-period 60))
       (loop)]
      [other other])))

(define (help!:continue? notification prompt)
  #;
  (notify-phone! notification)
  (user-prompt! prompt))

;; Ask if the user wants to remove the experiment-dir after a failed run
(define (prompt-for-cleanup!)
  (when (user-prompt! (format "Since the experiment failed, do you want to remove the experiment-dir? (~a)"
                              (current-experiment-dir)))
    (delete-directory (current-experiment-dir))))

(define (check-host-empty! host
                           [handle-not-empty!
                            (λ _
                              (unless (help!:continue?
                                       @~a{Host @host was not left in a clean state, stuck}
                                       @~a{
                                           Unexpected dirty state on @host, summary:
                                           @(pretty-format (summarize-experiment-status host))
                                           Is it fixed now? (Say no to abort)
                                           })
                                (raise-user-error 'check-host-empty! "Aborted."))
                              (check-host-empty! host))])
  (option-let*
   ([summary (summarize-experiment-status host)])
   (unless (summary-empty? summary)
     (handle-not-empty!))))

(define (setup-dbs! a-host
                    db-setup-script-name ; assumed to be in bex/orchestration/db-setup
                    [only-benchs #f] ; by default, sets up for all benchmarks in experiment-benchmarks
                    [handle-failure! (λ (reason)
                                       (raise-user-error 'update-host reason))]) 
  (unless (current-experiment-dir)
    (raise-user-error
     'setup-dbs!
     "No experiment config selected, `current-experiment-id` is not configured."))
  (define host-dbs-dir
    (build-path (current-experiment-dir) "dbs"))
  ;; TODO configure num_cores
  (define cmd @~a{
                  @(get-field host-racket-path a-host) -l 'bex/orchestration/db-setup/@db-setup-script-name' @;
                    -- @(string-join (map (λ (b) (format "-b ~a" b)) (or only-benchs '())) " ") @;
                       -x '@(current-experiment-dir)' @;
                       --no-viz @;
                       '@host-dbs-dir'
                  })
  (printf "Setting up DBs on host '~a'\n" a-host)
  (unless (send a-host
                  system/host
                  cmd
                  #:interactive? #t)
      (handle-failure! @~a{DB setup failed on host '@a-host'})))

(define (update-host! a-host
                      setup-config-name ; assumed to be in bex/setup/
                      #:skip-recompile? skip-recompile?
                      [handle-failure! (λ (reason)
                                         (raise-user-error 'update-host! reason))])
  (define host-repo-path
    (build-path (get-field host-project-path a-host)
                "contracts-and-testing"))
  (define host-benchmarks-path
    (build-path (get-field host-project-path a-host)
                "gtp-benchmarks"))
  (define host-setup-config-path
    (build-path (get-field host-project-path a-host)
                "contracts-and-testing"
                "bex"
                "setup"
                setup-config-name))
  (for ([step (in-list (list "Updating implementation..."
                             "Updating benchmarks..."
                             (if skip-recompile? "Skipping recompilation..." "Recompiling...")
                             "Checking status:"
                             "Enumerating tests in benchmarks..."))]
        [cmd (in-list
              (list
               @~a{
                   cd '@host-repo-path' && @;
                   git pull && @;
                   echo "Done."
                   }
               @~a{
                   cd '@host-benchmarks-path' && @;
                   git pull && @;
                   echo "Done."
                   }
               (if skip-recompile?
                   @~a{
                       echo "Continuing."
                       }
                   @~a{
                       @(get-field host-racket-path a-host) -l bex/util/project-raco -- -Cc && @;
                       echo "Done."
                       })
               @~a{
                   @(get-field host-racket-path a-host) -l bex/setup/setup -- -c '@host-setup-config-path' -v && @;
                   echo "Done."
                   }
               @~a{
                   cd '@host-benchmarks-path' && @;
                   @(get-field host-racket-path a-host) -l bex/util/enumerate-tests-for-benchmarks.rkt -- --gtp-benchmarks-dir . @(string-join experiment-benchmarks) && @;
                   echo "Done."
                   }))])
    (displayln step)
    (unless (send a-host
                  system/host
                  cmd
                  #:interactive? #t)
      (handle-failure! @~a{Update failed on step '@step'}))))

(define (format-status a-host
                       [summary* (summarize-experiment-status a-host)]
                       [active+pending-jobs* (send a-host get-jobs 'both)])
  (option-let*
   ([summary summary*]
    [active+pending-jobs active+pending-jobs*])

   (with-output-to-string
     (thunk
      (match-define (list active-job-ids pending-job-ids) active+pending-jobs)
      ;; listof(job-id, active?, progress, errorflag?)
      (define all-job-ids
        (set-union active-job-ids
                   pending-job-ids
                   (for*/list ([{_ jobs} (in-hash summary)]
                               [job-id* (in-list jobs)])
                     (match job-id*
                       [(list* bench mode _) (list bench mode)]))))
      (define all-job-info
        (for*/list ([maybe-job-id (in-list all-job-ids)]
                    [job-id (in-option maybe-job-id)])
          (define active? (member job-id active-job-ids))
          (define pending? (member job-id pending-job-ids))
          (define status (cond [active?  "R"]
                               [pending? "W"]
                               [else     "-"]))
          (match-define (list bench mode) job-id)
          (define-values {progress check-for-errors?}
            (match summary
              [(hash-table ['completed (list-no-order (== job-id) _ ...)]
                           _ ...)
               (values 1 #f)]
              [(hash-table [(and (or 'errored 'incomplete) category)
                            (list-no-order (list (== bench) (== mode) %) _ ...)]
                           _ ...)
               (values % (equal? category 'errored))]
              [(hash-table ['other (list-no-order (== job-id) _ ...)]
                           _ ...)
               (values 0 #t)]
              [else (values 0 #f)]))
          (list job-id status progress check-for-errors?)))
      (define (render-jobs jobs)
        (for ([job (in-list jobs)]
              [i (in-naturals)])
          (define prefix (if (zero? i) "" "\n         "))
          (match-define (list job-id status progress check-for-errors?) job)
          (display (~a prefix
                       (fixed-width-format job-id 60)
                       "  "
                       (match* {status progress}
                         [{"-" 1} "✓"]
                         [{"-" _} "?"]
                         [{"R" _} "R"]
                         [{"W" _} "W"])
                       "  "
                       (if (absent? progress)
                           "<no progress available>"
                           (render-progress progress status))
                       "  "
                       (if check-for-errors? "⚠" " ")))))
      (define (render-progress % status)
        (define bar-char (match status
                           ["R" #\▬]
                           [else #\▭]))
        (define end-char (match status
                           ["R" #\▶]
                           [else #\▭]))
        (define bar-size 20)
        (define progress-chars (inexact->exact
                                (round (* % bar-size))))
        (~a "▕"
            (fixed-width-format
             (match progress-chars
               [0 ""]
               [else (~a (make-string (sub1 progress-chars) bar-char) end-char)])
             bar-size)
            "▏"
            (if (= % 1)
                ""
                (~a (~r (* % 100) #:precision 1 #:min-width 4) "%"))))
      (define (fixed-width-format v width)
        (define content (~a v))
        (~a content (make-string (max 0 (- width (string-length content))) #\space)))
      (display "Active   ")
      (render-jobs (filter (match-lambda [(list _ "R" _ _) #t]
                                         [else #f])
                           all-job-info))
      (newline)
      (display "Pending  ")
      (render-jobs (filter (match-lambda [(list _ "W" _ _) #t]
                                         [else #f])
                           all-job-info))
      (newline)
      (display "Inactive ")
      (render-jobs (filter (match-lambda [(list _ "-" _ _) #t]
                                         [else #f])
                           all-job-info))
      (newline)))))

(define (launch-benchmarks! a-host config-name benchmark-names contract-levels
                            [handle-failure! (λ (benchmark)
                                               (displayln
                                                @~a{
                                                    Failed to submit job for @;
                                                    @benchmark @config-name on @a-host
                                                    }))]
                            #:outcome-checking-mode [outcome-checking-mode 'check])
  (for ([benchmark (in-list benchmark-names)]
        #:when #t
        [contract-setting (in-list contract-levels)]
        [i         (in-naturals)])
    ;; lltodo: the submission here can be batched
    ;; > This is (slightly) harder than the progress checks, just because of the job files.
    (when (and (not (zero? i))
               (zero? (modulo i 3)))
      (sleep (* 2 60)))
    (when (absent? (send a-host submit-job! benchmark config-name
                         #:contract-setting contract-setting
                         #:mode outcome-checking-mode))
      (handle-failure! benchmark))))


(define (notify-phone! msg)
  (system @~a{fish -c "notify-phone '@msg'"}))

(define (string->value s)
  (with-input-from-string s read))
(define (list->benchmark-spec l)
  (match l
    [(list host-sym config-name-sym benchmark-syms ...)
     (list (host-by-name (~a host-sym))
           (~a config-name-sym)
           (map ~a benchmark-syms))]
    [else (raise-user-error 'experiment-manager
                            @~a{Bad benchmark spec: @~v[l]})]))
(define (string->benchmark-spec s)
  (list->benchmark-spec (string->value s)))
(define (host-by-name name)
  (match (findf (λ (h) (string=? name (get-field hostname h))) hosts)
    [#f (raise-user-error 'experiment-manager @~a{No host known with name @name})]
    [host host]))
(define-simple-macro (pick-values f e)
  (call-with-values (thunk e)
                    (λ vals (f vals))))
(define ((mapper f) l) (map f l))


;; ----------------------------------------------------------------
;; ---------- Host options for orchestrating experiments ----------
;; ----------------------------------------------------------------

;; ----- these hosts should be used locally -----
(define zythos (new condor-host%
                    [hostname "zythos"]
                    [host-project-path "/project/teco"]
                    [host-jobdir-path "./proj/jobctl"]))
;; orchestrates/manages the experiment from the local machine, ssh'ing into zythos to
;; run the experiment, one mode at a time, on peroni -- offloading all mutants to condor
(define zythos-ssh/one-job-per-mutant
  (new direct-access-host%
       [hostname "zythos-direct"]
       [host-project-path "/project/teco"]
       [cpu-count 150]
       [env-vars "BEX_CONDOR_MACHINES='fix allagash piraat'"]))
(define benbox (new direct-access-host%
                    [hostname "benbox"]
                    [host-project-path "./teco"]))
(define local (new local-direct-host%
                   [cpu-count 2]
                   [hostname "local"]
                   ;; NOTE I think we'll need this if we ever need a remote host
                   #;[host-dbs-dir (simple-form-path (build-path project-path "/bex/dbs"))]
                   [host-project-path (simple-form-path project-path)]))

(define nathaniel (new local-direct-host%
                   [cpu-count 8]
                   [hostname "nathaniel"]
                   [host-project-path (simple-form-path project-path)]))

;; ----- these hosts should be used on peroni directly -----
;; runs the experiment with each mode as a whole machine condor job, all modes submitted at once
(define zythos-local/one-job-per-mode (new local-condor-host%
                          [hostname "zythos-local"]
                          [host-project-path (simple-form-path project-path)]
                          [host-jobdir-path (expand-user-path "~/proj/jobctl")]))
;; runs the experiment, one mode at a time, on peroni, offloading all mutants to condor
;; cpu-count is how many cpus the mode-experiment gets to use
;; (i.e. `cpu-count / batch-size` condor jobs)
(define zythos-local/one-job-per-mutant/batched
  (new local-direct-host%
       [cpu-count 10]
       [hostname "zythos-local-batch"]
       [host-project-path (simple-form-path project-path)]
       [env-vars "BEX_CONDOR_MACHINES='fix allagash piraat maudite tremens guldendraak' BEX_CONDOR_BATCH_SIZE=5"]))

(define zythos-local/one-job-per-mutant
  (new local-direct-host%
       [cpu-count 112]
       [hostname "zythos-local"]
       [host-project-path (simple-form-path project-path)]
       [env-vars "BEX_CONDOR_MACHINES='fix allagash piraat'"]))


(define hosts (list zythos-local/one-job-per-mutant/batched))
