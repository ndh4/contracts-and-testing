#lang at-exp racket

(require "./path-utils.rkt"
         "../configurations/configure-benchmark.rkt"
         db)

(provide (all-defined-out))

(define current-test-id (make-parameter #f))
(define current-test-type (make-parameter #f))

(struct/contract target-file ([name string?] [info any/c]))
(struct/contract context ([predecessor exact-integer?] [datum cons?] [info any/c]))
(struct/contract test ([context exact-integer?] [datum cons?] [info any/c]))

(define/contract (vector->record vec)
  (-> vector? (or/c target-file? context? test?))

  (define (v idx)
    (vector-ref vec idx))
  (match (v 1)
    ["target-file" (target-file (v 3) (v 4))]
    ["context" (context (v 2) (with-input-from-string (v 3) read) (v 4))]
    ["test" (test (v 2) (with-input-from-string (v 3) read) (v 4))]))

(define (sanitize-table-name name)
  (string-replace (path->string (path-replace-extension name ""))
    "-" "_"))

(define (benchmark->testable-modules a-benchmark)
  ; could have separate part of the struct for 'tests' folder,
  ; but for now just assume that 'untyped' will work
  (map file-name-string-from-path (benchmark-untyped a-benchmark)))

(define (get-all-test-ids test-type mod-name benchmark)
  (define mod-path (get-mod-path mod-name benchmark))
  (define dbc (get-test-dbc mod-path test-type))

  (dynamic-wind
   void
   (thunk
    (query-list
     dbc
     (format "SELECT id FROM ~a WHERE type=$1"
             (sanitize-table-name mod-name)) "test"))
   (thunk
    (disconnect dbc))))

(define (get-mod-path mod-name bench)
  (findf (path-ends-with mod-name) (benchmark-untyped bench)))

(define (get-test-dbc mod-path test-type)
  (define-values (base-dir file-name _) (split-path mod-path))
  (define-values (new-base __ ___) (split-path base-dir))

  (sqlite3-connect #:database (path->string (cleanse-path (path->complete-path (build-path new-base (format "~a-tests" test-type) "test-info.sqlite3"))))
                   #:mode 'read-only))

(define (get-record test-id test-dbc mod-name)
  (vector->record
    (query-row
      test-dbc
      (format "SELECT * FROM ~a WHERE id=$1"
        (sanitize-table-name mod-name)) test-id)))

(define (wrap-test test)
  (define datum (test-datum test))
  `(parameterize ([current-check-around (lambda (t)
                                          (t)
                                          (void))])
     ,datum))

(define (assemble-test test-id test-dbc mod-name #:rest [accum null])
  (define record (get-record test-id test-dbc mod-name))
  (cond
    [(target-file? record) accum]
    [else
     (define get-pred (if (context? record) context-predecessor test-context))
     (define get-datum (if (context? record) context-datum wrap-test))
     (assemble-test (get-pred record) test-dbc mod-name #:rest (cons (get-datum record) accum))]))

(define (lookup-test test-id module-path test-type)
  (define mod-name (path-replace-extension (file-name-from-path module-path) #""))
  (define test-dbc (get-test-dbc module-path test-type))
  (define test-as-list
    (dynamic-wind
     void
     (thunk
      (assemble-test test-id test-dbc mod-name))
     (thunk (disconnect test-dbc))))

  (cons 'begin test-as-list))
