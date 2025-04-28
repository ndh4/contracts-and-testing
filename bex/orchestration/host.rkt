#lang at-exp rscript

(provide host<%>
         host%
         with-temp-file)

(require syntax/parse/define
         "host-utils.rkt"
         "../util/option.rkt"
         "experiment-info.rkt")

(define-runtime-paths
  [store-path "../../../experiment-data/experiment-manager"]
  [std-experiment-runner-template "./standard-experiment-runner-script-template.sh"])

(define host<%> (interface (writable<%>)
                  ;; provided by host%
                  [system/host (unconstrained-domain-> boolean?)]
                  [system/host/string (unconstrained-domain-> string?)]
                  [scp (->*m {}
                             {#:from-host (or/c path-string? #f)
                              #:to-local (or/c path-string? #f)
                              #:from-local (or/c path-string? #f)
                              #:to-host (or/c path-string? #f)}
                             natural?)]

                  ;; may be implemented, must be called before submitting or canceling jobs
                  ;; is idempotent
                  [setup-job-management! (->m any)]

                  ;; must be implemented
                  [get-jobs
                   (let ([job-descr/c (listof (option/c (list/c string? string?)))])
                     (->*m {}
                           {(or/c boolean? 'both)}
                           (option/c
                            (or/c job-descr/c
                                  (list/c job-descr/c job-descr/c)))))]
                  [submit-job! (->*m {string?
                                      string?}
                                     {#:mode (or/c 'check 'record)
                                      #:cpus (or/c "decide" natural?)
                                      #:name string?}
                                     any)]
                  [cancel-job! (->m string? string? any)]))

(define host%
  (class object%
    (super-new)
    (init-field hostname
                host-project-path)
    (field [data-store-path (build-path store-path (~a hostname ".rktd"))]
           [host-racket-path (build-path host-project-path "racket" "bin" "racket")]
           [host-utilities-path
            (build-path host-project-path "contracts-and-testing" "bex" "util")]
           [host-data-path (build-path host-project-path "experiment-output")]
           [host-experiment-runner-script-path
            (build-path host-project-path "generated-run-experiment.sh")]
           [host-experiment-runner-script-uploaded? #f])
    (define/public (custom-write port) (write hostname port))
    (define/public (custom-display port) (display hostname port))

    (define/public (setup-job-management!) (void))

    (define/public (system/host #:interactive? [interactive? #f] . parts)
      ;; lltodo: implement a persistent connection here to prevent being blocked
      ;; by zythos for opening too many connections too quickly
      ;; > Tried this and gave up after a few hours. It's hard.
      ;; Instead, I should think about batching these calls higher up in the logic.
      (define cmd-str (apply ~a (add-between parts " ")))
      (define cmd-str-escaped (string-replace cmd-str "\"" "\\\""))
      (system @~a{ssh @(if interactive? "-t" "") @hostname "@cmd-str-escaped"}))

    (define/public (system/host/string #:interactive? [interactive? #f] . parts)
      (define out-str (open-output-string))
      (parameterize ([current-output-port out-str]
                     [current-error-port out-str])
        (send this system/host #:interactive? interactive? . parts))
      (get-output-string out-str))

    (define/public (scp #:from-host [from-host-path #f]
                        #:to-local [to-local-path #f]
                        #:from-local [from-local-path #f]
                        #:to-host [to-host-path #f])
      (system/exit-code
       @~a{scp -q @(match* {from-host-path
                            to-local-path
                            from-local-path
                            to-host-path}
                     [{from-remote to-local #f #f}
                      @~a{@|hostname|:'@from-remote' '@to-local'}]
                     [{#f #f from-local to-remote}
                      @~a{'@from-local' @|hostname|:'@to-remote'}]
                     [{_ _ _ _} (raise-user-error 'scp "Bad argument combination")])}))
    (define/public (upload-experiment-script!)
      (or host-experiment-runner-script-uploaded?
          (with-temp-file run-experiment.sh
            (display-to-file
             (string-replace (file->string std-experiment-runner-template)
                             "<<project-path>>"
                             (~a host-project-path))
             run-experiment.sh
             #:exists 'replace)
            (and/option
             (check-success
              (send this scp
                    #:from-local run-experiment.sh
                    #:to-host host-experiment-runner-script-path))
             (check-success
              (send this system/host @~a{chmod u+x @host-experiment-runner-script-path}))
             (set! host-experiment-runner-script-uploaded? #t)))))

    (define/public (make-experiment-runner-script-args benchmark
                                                       config-name ; without .rkt
                                                       record/check-mode
                                                       cpus
                                                       name)
      @~a{
          '@benchmark' @;
          '@|config-name|.rkt' @;
          '@record/check-mode' @;
          '@(current-remote-host-db-installation-directory-name)' @;
          '@cpus' @;
          '@name'
          })

    (define/public (ensure-store!)
      (make-directory* (path-only data-store-path))
      (unless (file-exists? data-store-path)
        (system @~a{touch '@data-store-path'})))
    (define/public (read-data-store)
      (ensure-store!)
      (file->list data-store-path))
    (define/public (write-data-store! data)
      (display-lines-to-file (map ~s data)
                             data-store-path
                             #:exists 'replace))))