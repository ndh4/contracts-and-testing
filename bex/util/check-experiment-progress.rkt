#lang at-exp rscript

(require db
         "../configurables/configurables.rkt"
         (submod "../experiment/mutant-factory.rkt" test)
         "../experiment/mutant-factory-data.rkt"
         "../configurations/configure-benchmark.rkt"
         "tests.rkt"
         "mutant-util.rkt"
         "option.rkt"
         (only-in "../orchestration/experiment-info.rkt" current-experiment-dir))

(define-runtime-paths
  [configs-dir "../configurables"]
  [experiment-launch-dir "../../.."])

(define (penultimate-path-element path)
  (cadr (reverse (explode-path path))))

(define (final-path-element path)
  (define-values (_base name _must-be-dir?) (split-path path))
  name)

(define (guess-path #:fail-thunk fail-f . parts)
  (define path (apply build-path parts))
  (if (or (path-to-existant-directory? path)
          (path-to-existant-file? path))
      path
      (fail-f path)))

(define (infer-benchmark log-path #:fail-thunk fail-thunk)
  (define benchmark-path
    (match (system/string @~a{grep -E 'Running on benchmark' @log-path})
      [(regexp #px"(?m:Running on benchmark (.+)$)" (list _ path))
       #:when (path-to-existant-directory? path)
       path]
      [(regexp #px"(?m:Running on benchmark #s\\(benchmark (.+)$)" (list _ benchmark-parts-str))
       (define benchmark-parts (call-with-input-string (~a "(" benchmark-parts-str) read))
       (define a-mod-path-symbol (first (first benchmark-parts)))
       (simple-form-path (build-path (~a a-mod-path-symbol) ".." ".."))]
      [other
       (fail-thunk)]))
  (read-benchmark benchmark-path))

(define (infer-configuration log-path #:fail-thunk fail-thunk)
  (match (system/string @~a{grep -E 'Running experiment with config' @log-path})
    [(regexp #rx"(?m:config (.+) and contract level (.+)$)" (list _ path))
     #:when (path-to-existant-file? path)
     path]
    [(regexp #rx"(?m:config (.+) and contract level (.+)$)" (list _ path _))
     #:when (path-to-existant-file? (build-path experiment-launch-dir path))
     (build-path experiment-launch-dir path)]
    [(regexp #rx"(?m:config .+/(.+) and contract level (.+)$)" (list _ config-name _))
     #:when (path-to-existant-file? (build-path configs-dir config-name))
     (build-path configs-dir config-name)]
    [else
     (fail-thunk)]))

(struct mutant*test (mutant test-mod test-id))

(define (all-mutant*tests-for bench #:with-sanity-checks? with-sanity-checks?)
  (define select-mutants (configured:select-mutants))
  (define (select-mutants/0-for-sanity-checks module-to-mutate-name)
    (if module-to-mutate-name (select-mutants module-to-mutate-name bench) '(0)))
  (for*/list ([module-to-mutate-name
               (in-list (if with-sanity-checks?
                            (cons #f (benchmark->mutatable-modules bench))
                            (benchmark->mutatable-modules bench)))]
              [mutation-index (select-mutants/0-for-sanity-checks module-to-mutate-name)]
              [module-under-test-name (in-list (benchmark->testable-modules bench))]
              [test-index (get-all-test-ids module-under-test-name bench)])
    (mutant*test (mutant module-to-mutate-name mutation-index #t) module-under-test-name test-index)))

#;
(define (check-progress-percentage progress-log-path all-mutants)
  (define progress (file->list progress-log-path))
  (/ (length progress)
     ;; TODO what is sample size?
     (* (sample-size) (length all-mutants))))

(define/contract (configuration-string-matches ctc-level)
  (-> (or/c "max" "types" "none") string?)
  (case ctc-level
    [("max")   "configuration=22222222 OR configuration=2222222 OR configuration=222222 OR configuration=22222 OR configuration=2222 OR configuration=222 OR configuration=22 OR configuration=2"]
    [("types") "configuration=11111111 OR configuration=1111111 OR configuration=111111 OR configuration=11111 OR configuration=1111 OR configuration=111 OR configuration=11 OR configuration=1"]
    [("none")  "configuration=0"]))

;; Query the number of rows in a sqlite table (0 if the table does not exist)
(define/contract (num-rows dbc table-name ctc-level)
  (connection? string? . -> . natural-number/c)
  (if (table-exists? dbc table-name)
      (query-value
       dbc
       (format
        "SELECT COUNT(*) FROM ~a WHERE ~a"
        table-name
        (configuration-string-matches ctc-level)))
      0))

;; FIXME this will break if dbc is not a connection. A better approach would be
;; have a db setup function that returns a set of functions to modify the db,
;; but never actually hand the user control of the db
(define/contract (check-progress-percentage/dbc dbc bench-name ctc-level all-mutant*tests)
  (connection? string? (listof mutant*test?) . -> . (and/c real? (not negative?) (<=/c 1)))
  ;; GROSS HACK there should really be a global enumeration of the configs that
  ;; will run for a given experiment. As it stands, adding a config in
  ;; experiment-manager does not get reflected here, so we will get progress
  ;; values greater than 1
  (/ (num-rows dbc bench-name ctc-level)
     (length all-mutant*tests)))

(define (progress-bar-string % #:width width)
  (define head-pos (inexact->exact (round (* % width))))
  (define before-head (make-string head-pos #\=))
  (define after-head (make-string (- width head-pos) #\space))
  (~a before-head ">" after-head))

(define (format-updating-display #:width width . parts)
  (define full (apply ~a parts))
  (define len (string-length full))
  (define remaining (- width len))
  (if (negative? remaining)
      (~a (substring full 0 (- width 3)) "...")
      (~a full (make-string remaining #\space))))

(define (truncate-string-to str chars)
  (if (<= (string-length str) chars)
      str
      (substring str 0 chars)))

(main
 #:arguments {[(hash-table ['watch watch-mode?]
                           ['log-name log-names]
                           ['readable-output? readable-output?])
               bench-con-level-dirs]
              #:once-each
              [("-w" "--watch")
               'watch
               ("Interactively show a progress bar that updates every 5 sec."
                "Only works with a single bench-con-level-dir.")
               #:record]
              [("-r" "--readable")
               'readable-output?
               ("Output information in a `read`able format.")
               #:record]
              #:multi
              [("-l" "--log-name")
               'log-name
               ("Explicitly provide the log file name. (one per bench-con-level-dir)")
               #:collect {"path" cons empty}]
              ;; Benchmark directories in experiment-output. Need log in
              ;; directory to infer configuration
              #:args bench-con-level-dirs}
 #:check [(andmap path-to-existant-directory? bench-con-level-dirs)
          @~a{Unable to find @(filter-not path-to-existant-directory? bench-con-level-dirs)}]
 #:check [(not (and watch-mode? (not (= (length bench-con-level-dirs) 1))))
          @~a{Watch mode can only be specified with a single bench-con-level-dir.}]

 (define %s
   (for/list ([bench-con-level-dir (in-list bench-con-level-dirs)]
              [log-name (in-sequences log-names (in-cycle (in-value #f)))])
     (option-let*
      ([log-path
        (guess-path bench-con-level-dir (or log-name (~a (penultimate-path-element bench-con-level-dir) ".log"))
                    #:fail-thunk
                    (λ (path)
                      (if readable-output?
                          absent
                          (raise-user-error
                           'guess-path
                           @~a{Unable to infer log path. Guessed: @path}))))]
       [bench
        (infer-benchmark
         log-path
         #:fail-thunk (thunk
                       (if readable-output?
                           absent
                           (raise-user-error
                            'check-experiment-progress
                            @~a{
                                Unable to infer benchmark path. @;
                                Are you running from the same directory the experiment was run?
                                }))))]

       [ctc-level (final-path-element bench-con-level-dir)]

       [config-path
        (infer-configuration
         log-path
         #:fail-thunk (thunk
                       (if readable-output?
                           absent
                           (raise-user-error
                            'check-experiment-progress
                            "Unable to infer path to config for this experiment."))))]

       ;; assume that the experiment-dir is three levels upward of the bench-con-level-dir
       [experiment-dir
        (simple-form-path (build-path bench-con-level-dir 'up 'up 'up))]

       [_ (begin
            (parameterize ([current-experiment-dir experiment-dir])
              (install-configuration! config-path))
            (unless readable-output?
              (displayln @~a{
                             Inferred benchmark @(benchmark->name bench) @;
                             and config @(find-relative-path (simple-form-path configs-dir)
                                                             (simple-form-path config-path))
                             })))]

       [all-mutant*tests (all-mutant*tests-for bench #:with-sanity-checks? #t)]

       #;[progress-log-path
        (guess-path (path-replace-extension log-path "-progress.log")
                    #:fail-thunk
                    (λ (path)
                      (if readable-output?
                          absent
                          (raise-user-error
                           'guess-path
                           @~a{Unable to infer progress log path. Guessed: @path}))))])

      (define dbc ((configured:connect-to-db)))
      (cond [watch-mode?
             (define period 5)
             (define start-time (current-inexact-milliseconds))
             (define start-% (check-progress-percentage/dbc dbc (benchmark->name bench) ctc-level all-mutant*tests))
             (let loop ()
               (define % (check-progress-percentage/dbc dbc (benchmark->name bench) ctc-level all-mutant*tests))
               (define pretty-%
                 (truncate-string-to (~a (* (/ (truncate (* % 1000)) 1000.0) 100))
                                     4))
               (define completion-rate:%/s
                 (/ (- % start-%)
                    (/ (match (- (current-inexact-milliseconds)
                                 start-time)
                         [0 +inf.0]
                         [not-0 not-0])
                       1000.0)))
               (define remaining-% (- 1 %))
               (define remaining-time-estimate
                 (if (zero? completion-rate:%/s)
                     0
                     (inexact->exact (round (/ (/ remaining-% completion-rate:%/s) 60.0)))))
               (display
                (format-updating-display
                 #:width 80
                 @~a{[@(progress-bar-string % #:width 50)] @|pretty-%|%}
                 " "
                 remaining-time-estimate
                 " min left"))
               (display "\r")
               (unless (= % 1)
                 (sleep period)
                 (loop)))]
            [else
             (define % (exact->inexact (check-progress-percentage/dbc dbc (benchmark->name bench) ctc-level all-mutant*tests)))
             (if readable-output?
                 %
                 (displayln %))]))))
 (when readable-output?
   (writeln (for/list ([maybe-% (in-list %s)])
              (if (absent? maybe-%)
                  'absent
                  maybe-%)))))
