#lang racket

(require "enumerate-tests.rkt"
         "path-utils.rkt")

(provide (contract-out
          [split-up-all-benchmarks!
           (->i ([gtp-benchmarks-dir path-to-existant-directory?]
                 [benchmarks (gtp-benchmarks-dir)
                             (listof (and/c string?)
                                     (λ (bench)
                                       (path-to-existant-directory?
                                        (benchmark-src-dir (benchmark-dir gtp-benchmarks-dir bench)))))])
                [result any/c])]))

;; path helpers (specific to gtp-benchmarks directory structure)
(define (benchmark-dir gtp-benchmarks-dir bench)
  (build-path gtp-benchmarks-dir "benchmarks" bench))

(define (benchmark-src-dir bench-dir)
  (build-path bench-dir "original"))

(define (benchmark-test-dir bench-dir)
  (build-path bench-dir "hand-tests"))

(define (benchmark-nontest-dir bench-dir)
  (build-path bench-dir "untyped"))

(define (split-up-all-benchmarks! gtp-benchmarks-dir benchmarks)
  (define bench-dirs (map ((curry benchmark-dir) gtp-benchmarks-dir) benchmarks))
  (for ([bench-dir bench-dirs])
    (printf "Splitting up code in ~a\n" bench-dir)
    (split-up-code (benchmark-src-dir bench-dir)
                   (benchmark-test-dir bench-dir)
                   (benchmark-nontest-dir bench-dir))))

(module+ main
  (require racket/cmdline)
  (define gtp-benchmarks-dir (make-parameter #f))
  (define benchmarks (make-parameter #f))
  (command-line #:once-each
                [("-g" "--gtp-benchmarks-dir")
                 path
                 "Path to gtp-benchmarks repository. Mandatory."
                 (gtp-benchmarks-dir path)]
                #:args benchmarks-to-split-up
                (benchmarks benchmarks-to-split-up))
  (split-up-all-benchmarks! (gtp-benchmarks-dir) (benchmarks)))
