#lang at-exp rscript

(provide condor-host%)

(require "host-utils.rkt"
         "../util/option.rkt"
         "host.rkt")

;; if it's not running?, it's pending
(struct job (id running?) #:transparent) 
(define job-info? (or/c (list/c string? string?) 
                        (list/c string? string? real?)))


;; runs each mode as a job, submitting all modes at once
(define condor-host%
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
                   host-project-path
                   data-store-path
                   host-racket-path
                   host-utilities-path
                   host-output-path
                   host-experiment-runner-script-path)
    (init-field [host-jobdir-path "."])
    (field [host-jobfile-path (build-path host-jobdir-path "job.sub")]
           [enabled-machines '("fix" "allagash" "piraat")])

    (define/public (get-jobs [active? #t])
      (option-let*
       ([info (all-job-info)])
       (if (equal? active? 'both)
           (let-values ([{active not} (partition job-running? info)]
                        [{find-job*} (match-lambda [(job id _) (find-job id)])])
             (list (map find-job* active)
                   (map find-job* not)))
           (filter-map (match-lambda [(job id (== active?)) (find-job id)]
                                     [else #f])
                       info))))
    
    (define/private (all-job-info)
      (define condor-dump (system/host/string "condor_q"))
      (match condor-dump
        [(regexp "-- Schedd: peroni.cs.northwestern.edu")
         (define raw-info
           (regexp-match* @pregexp{([1_])\s+[1_]\s+1 (\d{7}\.0)}
                          condor-dump
                          #:match-select rest))
         (define all-jobs
           (for/list ([parts (in-list raw-info)])
             (job (second parts) (string=? (first parts) "1"))))
         (expunge-old-jobs! all-jobs)
         all-jobs]
        [else
         absent]))

    (define/private (expunge-old-jobs! current-job-infos)
      (ensure-store!)
      (define filtered
        (for/list ([{bench+config id} (in-dict (read-data-store))]
                   #:when (findf (match-lambda [(job (== id) _) #t]
                                               [else #f])
                                 current-job-infos))
          (cons bench+config id)))
      (write-data-store! filtered))

    (define/public (submit-job! benchmark
                                config-name ; without .rkt
                                #:contract-setting contract-setting
                                #:mode [record/check-mode 'check]
                                #:cpus [cpus "decide"]
                                #:name [name benchmark])
      (define job-uploaded?
        (with-temp-file job.sub
          ;; NOTE condor-host is currently unused, so this is not updated with
          ;; the most recent directory structure. Instead, we use
          ;; spawn-condor-mutant-runner in condor.rkt
          (display-to-file
           @~a{
               # Set the universe
               Universe = vanilla

               # Describe the target machine
               Requirements = (@(string-join (for/list ([name (in-list enabled-machines)])
                                               @~a{(Machine == "@|name|.cs.northwestern.edu")})
                                             " || "))

               Rank = TARGET.Mips
               Copy_To_Spool = False

               # Notification
               Notification = never

               # Set the environment
               Getenv = True

               Arguments = "@(make-experiment-runner-script-args benchmark config-name record/check-mode cpus contract-setting name)"
               Executable = @host-experiment-runner-script-path
               Error = condor-output.txt
               Output = condor-output.txt
               Log = condor-log.txt

               +IsWholeMachineJob = true
               +IsSuspensionJob = false

               Queue

               }
           job.sub
           #:exists 'replace)
          (check-success (scp #:from-local job.sub #:to-host host-jobfile-path))))
      (option-let* ([_ job-uploaded?]
                    [_ (upload-experiment-script!)]
                    [id (match (system/host/string
                                @~a{condor_submit -verbose @host-jobfile-path})
                          [(regexp @pregexp{\*\* Proc ([\d.]+):} (list _ id)) id]
                          [else absent])])
        (save-job! benchmark config-name id)))

    (define/public (cancel-job! benchmark config-name)
      (option-let* ([id (read-job benchmark config-name)])
                   (void (system/host @~a{condor_rm '@id'}))
                   (write-data-store! (dict-remove (read-data-store)
                                                   (list benchmark config-name)))))

    (define/private (save-job! benchmark config-name id)
      (ensure-store!)
      (with-output-to-file data-store-path #:exists 'append
        (thunk (writeln (cons (list benchmark config-name) id)))))
    
    (define/private (read-job benchmark config-name) ; -> (option/c id?)
      (dict-ref (read-data-store)
                (list benchmark config-name)
                absent))
    
    (define/private (find-job target-id) ; -> (option/c (list/c benchmark config-name))
      (ensure-store!)
      (for*/option ([{bench+config id} (in-dict (file->list data-store-path))]
                    #:when (string=? id target-id))
                   bench+config))))