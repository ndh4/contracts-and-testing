#lang at-exp rscript

;; project-raco has commands:
;; - make
;;   - compiles everything that is not a helper/utility script which can't be compiled on all systems (e.g. uses plot-util/quick)
;; - test
;;   - runs all tests, with the following extra features:
;;     - progress tracking and the ability to resume tests upon failure
;;     - running tests in parallel when possible
;;       + Specifically, we assume modules in different directories are safe to run in parallel
;; - progress
;;   - uses *-progress.log files to report on the progress of operations
;;   - for the main experiment, it can count how many mutants have completed

(require setup/getinfo)

(define-runtime-paths [contracts-and-testing "../"])
(define patterns-to-ignore ((get-info/full contracts-and-testing) 'compile-omit-paths))
(define (ignored-path? p)
  (ormap (λ (pat) (regexp-match? pat p)) patterns-to-ignore))
(define raco
  (build-path (let-values ([(base name must-be-dir?) (split-path (simple-form-path (find-system-path 'exec-file)))])
                base)
              "raco"))

(main
 #:arguments
 {[(hash-table ['compile compile?] ['clean-compiled clean-compiled?]) args]
  #:once-each [("-c" "--compile") 'compile "Compile project code." #:record]
  [("-C" "--clean-compiled") 'clean-compiled "Clean all previously compiled project code." #:record]}
 (when clean-compiled?
   (system* (find-executable-path "find")
            contracts-and-testing
            "-name"
            "compiled"
            "-type"
            "d"
            "-prune"
            "-exec"
            "rm"
            "-rf"
            "{}"
            ";"))
 (when compile?
   (parameterize ([current-directory contracts-and-testing])
     (apply system*
            raco
            "make"
            "-v"
            (for/list ([f (in-directory)]
                       #:when
                       (and (path-has-extension? f ".rkt") (file-exists? f) (not (ignored-path? f))))
              f)))))
