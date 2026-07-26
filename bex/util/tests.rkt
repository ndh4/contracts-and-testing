#lang at-exp racket

(require "./path-utils.rkt"
         "../configurations/configure-benchmark.rkt")

(provide (all-defined-out))

(define current-test-id (make-parameter #f))
(define current-test-type (make-parameter #f))

(struct target-file [name info] #:prefab)
(struct context [predecessor datum info] #:prefab)
(struct test [context datum info] #:prefab)

(define (benchmark->testable-modules a-benchmark)
  ; could have separate part of the struct for 'tests' folder,
  ; but for now just assume that 'untyped' will work
  (map file-name-string-from-path (benchmark-untyped a-benchmark)))

(define (get-all-test-ids test-type mod-name benchmark)
  (define mod-path (get-mod-path mod-name benchmark))
  (define db-hash (get-test-database mod-path test-type))

  (for/fold ([accum '()]) ([(key val) db-hash])
    (if (test? val)
        (cons key accum)
        accum)))

(define (get-mod-path mod-name bench)
  (findf (path-ends-with mod-name) (benchmark-untyped bench)))

(define (get-test-database mod-path test-type)
  (define-values (base-dir file-name _) (split-path mod-path))
  (define-values (new-base __ ___) (split-path base-dir))

  (file->value (build-path new-base (format "~a-tests" test-type) (path-replace-extension file-name #".rktd"))))

(define (get-record test-id test-database)
  (hash-ref test-database test-id))

(define (wrap-test test)
  (define datum (test-datum test))
  `(parameterize ([current-check-around (lambda (t)
                                          (t)
                                          (void))])
     ,datum))

(define (assemble-test test-id test-database #:rest [accum null])
  (define record (get-record test-id test-database))
  (cond
    [(target-file? record) accum]
    [else
     (define get-pred (if (context? record) context-predecessor test-context))
     (define get-datum (if (context? record) context-datum wrap-test))
     (assemble-test (get-pred record) test-database #:rest (cons (get-datum record) accum))]))

(define (lookup-test test-id module-path test-type)
  (define test-as-list (assemble-test test-id (get-test-database module-path test-type)))

  (cons 'begin test-as-list))
