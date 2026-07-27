#lang racket

(require "enumerate-tests.rkt")

(define level 'max)

(define benchmark-name "abm_test")

(define benchmark-dir
  (build-path
   "/Users/nhejduk/Documents/Research-Cloud/teco-parent/gtp-benchmarks/benchmarks"
   benchmark-name))

(define wiretap-results-dir
  (build-path benchmark-dir (format "wiretap-~a-results" level)))

(define sourcecode-dir
  (build-path benchmark-dir "original"))

(define output-dir
  (build-path benchmark-dir (format "rand_~a-tests" level)))

(define/contract
  (get-module-from-wiretap-result-name path)
  (-> (and/c path-string?
             (lambda (p) (regexp-match? #rx"^(.+)_TAP_(.+)rktd$" p)))
      string?)
  (match (regexp-match #rx"^(.+)_TAP_(.+)rktd$" (path->string path))
    [(list _ name _) (format "~a.rkt" name)]))

(define (to-hash get-key lst)
  (for/fold ([acc (hash)])
            ([item (in-list lst)])
    (hash-update acc
                 (get-key item)
                 (λ (current-list)
                   (cons item current-list))
                 '())))

(define (make-testcases-obj source-file module-to-captureds)
  (define hash-start (hash 0 (target-file (~a sourcecode-dir) '())))
  (define hash-with-context
    (hash-set hash-start 1
              (context 0 '(begin
                            (require rackunit)
                            (require "../../../wiretapping/wiretap.rkt")
                            (define-namespace-anchor teco_namespace_anchor)) '())))
  (define testcase-files (hash-ref module-to-captureds (path->string source-file) '()))
  (for/fold ([ht hash-with-context]
             [next-index 2]
             #:result ht)
            ([input-filename testcase-files])
    (define input (build-path wiretap-results-dir input-filename))
    (with-input-from-file input
      (lambda ()
        (let read-loop ()
          (let ([line (read-line)])
            (cond
              [(eof-object? line) (values ht next-index)]
              [(string=? line "(begin-random-tests)")
               (write-loop ht next-index)]
              [else (read-loop)])))))))

(define (write-loop ht next-id)
  (define line (read))
  (cond
    [(eof-object? line) (values ht next-id)]
    [(eq? 'call (car line))
     (write-loop
      (hash-set ht next-id (test 1 `(execute-call/namespace ,line (namespace-anchor->namespace teco_namespace_anchor)) '()))
      (add1 next-id))]
    [else
     (write-loop ht next-id)]))

(define (process-module source-file module-to-captureds)
  (define obj (make-testcases-obj source-file module-to-captureds))
  (with-output-to-file
      (build-path output-dir
                  (path-replace-extension source-file ".rktd"))
    (thunk (pretty-write obj)) #:exists 'replace))


(define (get-result-files dir-name)
  (filter
   (lambda (p) (regexp-match? #rx"^(.+)_TAP_(.+)rktd$" p))
   (directory-list wiretap-results-dir)))

(create-dir-if-not-exists! output-dir)

(define module-to-captureds
  (to-hash get-module-from-wiretap-result-name
           (get-result-files wiretap-results-dir)))

(for ([source-file (directory-list sourcecode-dir)])
  (process-module source-file module-to-captureds))