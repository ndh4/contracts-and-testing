#lang at-exp rscript


(provide direct-access-host%)

(require "../util/option.rkt"
         "host-utils.rkt"
         "host.rkt")

(define-logger experiment-manager)
(define recvr (make-log-receiver experiment-manager-logger 'debug))

(void
 (thread
  (lambda () (let loop()
               (define v (sync recvr))
               (printf "[~a] ~a~n" (vector-ref v 0) (vector-ref v 1))
               (loop)))))

;; runs one mode of the experiment at a time, each having all `cpu-count` cpus
(define direct-access-host%
  (class* host% (writable<%> host<%>)
    (super-new)
    (inherit system/host
             system/host/string
             scp
             upload-experiment-script!
             make-experiment-runner-script-args
             ensure-store!
             read-data-store
             write-data-store!)
    (inherit-field hostname
                   data-store-path
                   host-project-path
                   host-racket-path
                   host-utilities-path
                   host-output-path
                   host-experiment-runner-script-path)
    (init-field [cpu-count 1]
                [env-vars ""])
    (field [run-screen-name "experiment-run"]
           [management-screen-name "experiment-manage"]
           [queueing-thd #f])

    (define/override (setup-job-management!)
      (unless (thread? queueing-thd)
        (when (and (not (empty? (read-data-store)))
                   (user-prompt!
                    @~a{
                        Found existing job data in persistent queue @;
                        (at @data-store-path). @;
                        Do you want to clear it before continuing? @;
                        (Answering no means the old job data will be executed *before* any new.)
                        }))
          (write-data-store! empty))
        (set! queueing-thd (make-direct-access-host-queue-manager))))

    
    (define/public (get-jobs [active? #t] #:with-pid? [with-pid? #f])
      (define experiment-script-name (basename host-experiment-runner-script-path))
      (option-let*
       ([active (match (system/host/string (format "ps -ef | grep ~a" experiment-script-name) ;@~a{ps -ef | grep @experiment-script-name}
       )
                  [(regexp (pregexp @~a{(?m:^\s*\S+\s+(\d+)\s+(\S+\s+){5}.*bash .*@experiment-script-name (\S+) (\S+).rkt)})
                           (list _ script-pid _ benchmark config-name))
                   (log-experiment-manager-debug "Happy Case")
                   ;; we found the running experiment script
                   (list (list* benchmark
                                config-name
                                (if with-pid?
                                    ;; get the mutant-factory pid, since that's what actually needs to be killed to cancel the job
                                    (regexp-match* (pregexp @~a{\s(\d+)\s+@script-pid .*mutant-factory.rkt})
                                                   (system/host/string (format "ps -ef | grep ~a" script-pid) ;@~a{ps -ef | grep @script-pid}
                                                   )
                                                   #:match-select cadr)
                                    empty)))]
                  [(regexp (pregexp @~a{grep[^@"\n"]+@experiment-script-name})) 
                   ;; we only found the grep
                   (log-experiment-manager-debug "Empty Case")
                   empty]
                  [else
                   ;; we didn't even find the grep for some reason
                   (log-experiment-manager-debug "Absent Case") absent])])

       (match active?
         [#t active]
         ['both (list active (get-queued-jobs))]
         [else (get-queued-jobs)])))

    (define/private (get-queued-jobs)
      (map (match-lambda [(list* benchmark config-name _) (list benchmark config-name)])
           (read-data-store)))

    (define/public (submit-job! benchmark
                                config-name ; without .rkt
                                #:contract-setting contract-setting
                                #:test-type test-type
                                #:mode [record/check-mode 'check]
                                #:cpus [cpus cpu-count]
                                #:name [name benchmark])
      (setup-job-management!)
      (option-let*
       ([_ (thread-send queueing-thd
                        `(submit ,(list benchmark config-name record/check-mode cpus contract-setting test-type name))
                        (thunk absent))])
       (thread-receive)))
    (define/public (cancel-job! benchmark config-name)
      (setup-job-management!)
      (option-let*
       ([_ (thread-send queueing-thd
                        `(cancel ,(list benchmark config-name))
                        (thunk absent))])
       (thread-receive)))

    (define/private (launch-job! benchmark
                                 config-name ; without .rkt
                                 record/check-mode
                                 cpus
                                 ctc-setting
                                 test-type
                                 name)
      (define run-cmd
        @~a{
            @env-vars @;
            '@host-experiment-runner-script-path' @;
            @(make-experiment-runner-script-args benchmark config-name record/check-mode cpus ctc-setting test-type name)
            })
      (log-experiment-manager-debug @~a{launching job with cmd: @run-cmd})
      (option-let*
       ([_ (ensure-screen-setup!)]
        [_ (upload-experiment-script!)]
        [_ (check-success
            (system/host @~a{screen -S @run-screen-name -p 0 -X stuff "@|run-cmd|$(printf \\r)"}))]
        [_ (log-experiment-manager-debug @~a{Job launched successfully})])
       (void)))
    (define/private (cancel-currently-running-job!)
      (option-let*
       ([active-jobs (get-jobs #t #:with-pid? #t)]
        [_ (bool->option (not (empty? active-jobs)))]
        [pid (match active-jobs
               [`((,_ ,_ ,pid)) pid]
               [else
                (log-experiment-manager-error
                 @~a{unexpected shape of active jobs: @~s[active-jobs]})
                absent])]
        [_ (log-experiment-manager-debug @~a{killing active job with pid @pid})]
        [_ (check-success (system/host @~a{kill @pid}))])
       (void)))

    (define/private (ensure-screen-setup!)
      (define screens-output (system/host/string "screen -ls"))
      (define (session-exists? name)
        (regexp-match? @~a{[0-9]+\.@name} screens-output))
      (define (launch-screen! name)
        (system/host @~a{screen -dmS @name bash}))
      (define run-ok?
        (unless (session-exists? run-screen-name) (launch-screen! run-screen-name)))
      (define management-ok?
        (unless (session-exists? management-screen-name) (launch-screen! management-screen-name)))
      (cond [(and run-ok? management-ok?) (void)]
            [else
             (displayln @~a{Failed to obtain necessary screen sessions})
             absent]))

    (define/private (make-direct-access-host-queue-manager)
      (define main-thd (current-thread))
      (thread
       (thunk
        (define message-evt (thread-receive-evt))

        (define (enqueue-job! spec)
          (with-data-store-lock
            (thunk (write-data-store! (append (read-data-store)
                                              (list spec))))
            (thunk (displayln @~a{Failed to enqueue job @spec, couldn't get data store lock}))))
        (define (cancel-job! id)
          (log-experiment-manager-debug @~a{canceling job @id})
          (match-define (list benchmark config-name) id)
          (with-data-store-lock
            (thunk
             (match (get-jobs #t)
               [(list running-job-id)
                #:when (equal? running-job-id id)
                (log-experiment-manager-debug @~a{... which is currently running})
                (cancel-currently-running-job!)]
               [(? list? running-jobs)
                (log-experiment-manager-debug @~a{... which is still in the queue (running: @running-jobs)})
                (define current-q (read-data-store))
                (define new-q (remf (match-lambda [(list* (== benchmark) (== config-name) _) #t]
                                                  [else #f])
                                    current-q))
                (write-data-store! new-q)]
               [other
                (displayln @~a{Failed to cancel job @id, current-jobs: @other})]))
            (thunk (displayln @~a{Failed to cancel job @id, couldn't get data store lock}))))

        (define (current-job-done?)
          (empty? (get-jobs #t)))

        (define (queue-empty?)
          (empty? (read-data-store)))

        (define (launch-next-job!)
          (log-experiment-manager-debug @~a{@hostname launching next job})
          (with-data-store-lock
            (thunk (define current-q (read-data-store))
                   (define new-q (rest current-q))
                   ;; This unpacking is necessary because apparently there's no
                   ;; way to do an `apply`-type application of a private method.
                   (match-define (list benchmark config-name record/check-mode cpus ctc-setting test-type name)
                     (first current-q))
                   (launch-job! benchmark config-name record/check-mode cpus ctc-setting test-type name)
                   (write-data-store! new-q))
            (thunk (displayln @~a{
                                  Warning: couldn't launch next job @;
                                  because couldn't get data store lock
                                  }))))

        (log-experiment-manager-debug @~a{@hostname direct-access queue thread launched})
        (let loop ()
          (match (thread-try-receive)
            [`(submit ,job-spec)
             (enqueue-job! job-spec)
             (thread-send main-thd #t)
             (log-experiment-manager-debug @~a{@hostname received @job-spec})]
            [`(cancel ,job-id)
             (cancel-job! job-id)
             (thread-send main-thd #t)
             (log-experiment-manager-debug @~a{@hostname canceled @job-id})]
            [else (void)])
          (if (and (current-job-done?)
                   (not (queue-empty?)))
              (launch-next-job!)
              (sync/timeout (* 5 60) message-evt))
          (loop)))))

    (define/private (with-data-store-lock thunk fail-thunk)
      (call-with-file-lock/timeout data-store-path
                                   'exclusive
                                   thunk
                                   fail-thunk
                                   #:max-delay 1))))