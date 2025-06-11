#lang at-exp racket

(provide (all-defined-out))

(define current-test-id (make-parameter #f))

(struct target-file [name info] #:prefab)
(struct context [predecessor datum info] #:prefab)
(struct test [context datum info] #:prefab)

(define (get-all-test-ids mod-path)
  (define db-hash (get-test-database mod-path))

  (for/fold ([accum '()]) ([(key val) db-hash])
    (if (test? val)
        (cons key accum)
        accum)))

(define (get-test-database mod-path)
  (define-values (base-dir file-name _) (split-path mod-path))
  (define-values (new-base __ ___) (split-path base-dir))

  (file->value (build-path new-base "tests" (path-replace-extension file-name #".rktd"))))

(define (get-record test-id test-database)
  (hash-ref test-database test-id))

(define (assemble-test test-id test-database #:rest [accum null])
  (define record (get-record test-id test-database))
  (cond
    [(target-file? record) accum]
    [else
     (define get-pred (if (context? record) context-predecessor test-context))
     (define get-datum (if (context? record) context-datum test-datum))
     (assemble-test (get-pred record) test-database #:rest (cons (get-datum record) accum))]))

(define (lookup-test test-id module-path)
  (define test-as-list (assemble-test test-id (get-test-database module-path)))

  (cons 'begin test-as-list))
