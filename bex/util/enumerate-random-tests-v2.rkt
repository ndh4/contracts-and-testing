#lang racket

(define (reader-loop #:in in #:initial-accum-val initial-accum-val #:on-expr on-expr)
  (let loop ([accum initial-accum-val])
    (define expr (read in))
    (cond [(eof-object? expr)
           accum]
          [else
           (loop (on-expr #:expr expr #:accum accum))])))

(define (find-contracted-identifiers file)
  (call-with-input-file file
    (lambda (in)
      (read-line in) ;; Ignore hashlang declaration
      (reader-loop
       #:in in
       #:initial-accum-val '()
       #:on-expr
       (lambda (#:expr expr #:accum accum)
         (match expr
           [(list* 'define/contract (cons (? symbol? id) _) _ _)
            ;; ^ Match shorthand function definitions: (define (id args ...) body ...)
            (cons id accum)]
           [else
            (when (string-contains? (~a expr) "define/contract")
              (printf "!Possible missed contract(s): ~a~n~n" expr))
            accum]))))))

(require "enumerate-tests.rkt"
         racket/runtime-path
         db)

(provide make-random-tests!)

(define benchmark-name (make-parameter 'bad))

(define-runtime-path BENCHMARKS "../../../gtp-benchmarks/benchmarks")

(define (benchmark-dir)
  (build-path
   BENCHMARKS
   (benchmark-name)))

(define (sourcecode-dir)
  (build-path (benchmark-dir) "original"))

(define (output-dir)
  (build-path (benchmark-dir) "rand-tests"))


(define (ensure-table! dbc table-name)
  (query-exec
   dbc
   (format "CREATE TABLE IF NOT EXISTS ~a (
   id INTEGER PRIMARY KEY,
   type TEXT,
   pred INTEGER,
   content TEXT,
   info TEXT
   )" table-name))
   dbc)

(define (add-record! dbc table-name id record)
  (match record
    [(target-file name info) (add-entry! dbc table-name id "target-file" sql-null name info)]
    [(context pred datum info) (add-entry! dbc table-name id "context" pred (~s datum) info)]
    [(test pred datum info) (add-entry! dbc table-name id "test" pred (~s datum) info)]))

(define SEED 43)

(define (process-module! source-file dbc)
  (define table-name (sanitize-table-name (file-name-from-path source-file)))
  (ensure-table! dbc table-name)

  (add-record! dbc table-name 0 (target-file (~a source-file) sql-null))
  (add-record! dbc table-name 1
    (context 0 '(begin
                  (require rackunit)
                  (define-namespace-anchor teco_namespace_anchor)) sql-null))

  (define source-path (build-path (sourcecode-dir) source-file))
  (define contracted-identifiers (find-contracted-identifiers source-path))

  (for/fold ([next-index 2])
            ([identifier contracted-identifiers])
    (for/last ([i (in-range 11)])
      (define id (+ next-index i))
      (define the-test (make-test-from-identifier identifier i 43))
      (add-record! dbc table-name id the-test)
      (add1 id))))

(define (make-test-from-identifier the-identifier fuel seed)
  (test 1 `(begin (random-seed ,seed) (parameterize ([current-pseudo-random-generator (current-contract-pseudo-random-generator)]) (random-seed ,seed)) (contract-exercise ,the-identifier #:fuel ,fuel)) sql-null))

(define (fresh-dbc)
  (create-dir-if-not-exists! (output-dir))
  (define out (build-path (output-dir) "test-info.sqlite3"))
  (when (file-exists? out) (delete-file out))
  (sqlite3-connect #:database out #:mode 'create))

(define (make-random-tests! benchmark)
  (printf "Collecting random tests for '~a'...~n" benchmark)
  (parameterize ([benchmark-name benchmark])
    (define dbc (fresh-dbc))
    (dynamic-wind
     void
     (thunk
      (for ([source-file (directory-list (sourcecode-dir))]
            #:when (equal? (path-get-extension source-file) #".rkt"))
        (process-module! source-file dbc)))
     (thunk (disconnect dbc)))))