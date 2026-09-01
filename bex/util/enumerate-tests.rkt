#lang racket

(require "read-module.rkt"
         "path-utils.rkt"
         "tests.rkt"
         db)

(provide (contract-out
          [split-up-code
           (-> path-to-existant-directory?
               path-string?
               path-string?
               any)]))

(provide (struct-out target-file)
         (struct-out context)
         (struct-out test)
         create-dir-if-not-exists!
         sanitize-table-name
         add-entry!)

(struct target-file [name info] #:prefab)
(struct context [predecessor datum info] #:prefab)
(struct test [context datum info] #:prefab)

(define (printr x)
  (printf ": ~a~n" x)
  x)

(define (make-db! path mod-name)
  (when (file-exists? path) (delete-file path))
  (define dbc (sqlite3-connect #:database path #:mode 'create))
  (query-exec
   dbc
   (format "CREATE TABLE ~a (
   id INTEGER PRIMARY KEY,
   type TEXT,
   pred INTEGER,
   content TEXT,
   info TEXT
   )" mod-name))
   dbc)

(define (add-entry! dbc mod-name id type pred content info)
  (query-exec
    dbc
    (format "INSERT INTO ~a
    (id, type, pred, content, info)
    VALUES($1, $2, $3, $4, $5)"
    mod-name)
    id type pred content info))

(define (into-records! in out mod-name)
  (define dbc (make-db! out mod-name))
  (define test-data (with-input-from-file in get-test-code))

  (add-entry! dbc mod-name 0 "target-file" sql-null (~a in) sql-null)
  (generate-records! test-data dbc mod-name 1 0))

(define (generate-records! test-data dbc mod-name next-id pred-id)
  (unless (null? test-data)
     (cond
       [(is-test? (first test-data))
        (add-entry! dbc mod-name next-id "test" pred-id (~s (first test-data)) sql-null)
        (generate-records! (rest test-data) dbc mod-name (add1 next-id) pred-id)]
       [else
        (define-values (ctxt rest-test-data) (consume-context test-data))
        (add-entry! dbc mod-name next-id "context" pred-id (~s (cons 'begin ctxt)) sql-null)
        (generate-records! rest-test-data dbc mod-name (add1 next-id) next-id)])))

; test-data -> (values
;                 extracted-context
;                 remaining-test-data)
(define (consume-context test-data)
  (cond
    [(or (null? test-data) (is-test? (first test-data))) (values '() test-data)]
    [else
     (define-values (ctxt rest-test-data) (consume-context (rest test-data)))
     (values (cons (first test-data) ctxt) rest-test-data)]))

(define (is-test? stx)
  (string-prefix? (symbol->string (first stx)) "check-"))

(define (get-test-code)
  (apply
   append
   (map cddr (filter (lambda (x) (and (eq? (first x) 'module+) (eq? (second x) 'test))) (get-data)))))

(define (create-dir-if-not-exists! path)
  (unless (directory-exists? path)
    (make-directory path)))

(define (split-up-code src-dir test-dir nontest-dir)
  (create-dir-if-not-exists! test-dir)
  (create-dir-if-not-exists! nontest-dir)

  (define src-files (find-files (lambda (path) (equal? (path-get-extension path) #".rkt")) src-dir))

  (for ([src-file src-files])
    (define name (file-name-from-path src-file))
    (when name
      (into-records! src-file (build-path test-dir "test-info.sqlite3") (sanitize-table-name name))
      (comment-out src-file
                   (build-path nontest-dir name)
                   (lambda (sexp)
                     (match sexp
                       [`(module+ test
                           ,_ ...)
                        #t]
                       [else #f]))))))

(define (comment-out input-file output-file comment-it?)
  (displayln input-file)
  (displayln output-file)
  (with-input-from-file input-file
                        (lambda ()
                          (with-output-to-file output-file
                                               #:exists 'replace
                                               (lambda ()
                                                 (displayln (read-line)) ;hashlang
                                                 (newline)
                                                 (define all-exprs (port->list))
                                                 (for ([expression all-exprs])
                                                   (when (comment-it? expression)
                                                     (display "#;"))
                                                   (pretty-write expression)
                                                   (newline)))))))

(module+ main
  (require racket/cmdline)
  (define source-dir (make-parameter #f))
  (define dest-dir (make-parameter #f))
  (define test-dir (make-parameter #f))
  (command-line #:once-each
                [("--source") path "Path to source directory. Mandatory." (source-dir path)]
                [("--test-dir") path "Path to test directory. Mandatory." (test-dir path)]
                [("--untyped-dest-dir")
                 path
                 "Path to source code destination directory. Mandatory."
                 (dest-dir path)])
  (split-up-code (source-dir) (test-dir) (dest-dir)))
