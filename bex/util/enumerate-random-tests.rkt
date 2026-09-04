#lang racket

(require "enumerate-tests.rkt"
         db)

(provide organize-random-tests!)

(define level 'max)

(define benchmark-name (make-parameter 'bad))

(define (benchmark-dir)
  (build-path
   "/Users/nhejduk/Research-Local/teco-parent/gtp-benchmarks/benchmarks"
   (benchmark-name)))

(define (wiretap-results-dir)
  (build-path (benchmark-dir) (format "wiretap-~a-results" level)))

(define (sourcecode-dir)
  (build-path (benchmark-dir) "original"))

(define (output-dir)
  (build-path (benchmark-dir) (format "rand_~a-tests" level)))

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

(define (in-table? dbc table-name condition)
  (not (zero?
    (query-value dbc
     (format "SELECT COUNT(*) FROM ~a WHERE ~a"
       table-name
       condition)))))

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

(define (process-module! source-file testcase-files dbc)
  (define table-name (sanitize-table-name (file-name-from-path source-file)))
  (ensure-table! dbc table-name)

  (add-record! dbc table-name 0 (target-file (~a source-file) sql-null))
  (add-record! dbc table-name 1
    (context 0 '(begin
                  (require rackunit)
                  (require "../../../wiretapping/wiretap.rkt")
                  (define-namespace-anchor teco_namespace_anchor)) sql-null))
  (for/fold ([next-index 2])
            ([input-filename testcase-files])
    (define input (build-path (wiretap-results-dir) input-filename))
    (with-input-from-file input
      (lambda ()
        (let read-loop ()
          (let ([line (read-line)])
            (cond
              [(eof-object? line) next-index]
              [(string=? line "(begin-random-tests)")
               (write-loop dbc table-name next-index)]
              [else (read-loop)])))))))

(define (make-test-from-call the-call)
  (test 1 `(execute-call/namespace ,the-call (namespace-anchor->namespace teco_namespace_anchor)) sql-null))

(define (write-loop dbc table-name next-id)
  (define line (read))
  (cond
    [(eof-object? line) next-id]
    [(eq? 'call (car line))
     (define the-test (make-test-from-call line))
     (cond
      [(and (equal? (cddr line) '((list) (list) (list)))
            (in-table? dbc table-name (format "content='~a'" (test-datum the-test))))
       ;; This call is a no-arg call already present in the table,
       ;; so skip it.
       (write-loop dbc table-name next-id)]
      [else
       (add-record! dbc table-name next-id the-test)
       (write-loop dbc table-name (add1 next-id))])]
    [else
     (write-loop dbc table-name next-id)]))


(define (get-result-files dir-name)
  (filter
   (lambda (p) (regexp-match? #rx"^(.+)_TAP_(.+)rktd$" p))
   (directory-list (wiretap-results-dir))))

(define (fresh-dbc)
  (create-dir-if-not-exists! (output-dir))
  (define out (build-path (output-dir) "test-info.sqlite3"))
  (when (file-exists? out) (delete-file out))
  (sqlite3-connect #:database out #:mode 'create))

(define (organize-random-tests! benchmark)
  (printf "Collecting random tests for '~a'...~n" benchmark)
  (parameterize ([benchmark-name benchmark])
    (define dbc (fresh-dbc))
    (define module-to-captured-ids
      (to-hash get-module-from-wiretap-result-name
               (get-result-files (wiretap-results-dir))))
    (dynamic-wind
     void
     (thunk
      (for ([source-file (directory-list (sourcecode-dir))]
            #:when (equal? (path-get-extension source-file) #".rkt"))
        (define testcase-files (hash-ref module-to-captured-ids (path->string source-file) '()))
        (process-module! source-file testcase-files dbc)))
     (thunk (disconnect dbc)))))