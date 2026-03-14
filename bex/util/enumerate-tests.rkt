#lang racket

(require "read-module.rkt")

(provide (struct-out target-file)
         (struct-out context)
         (struct-out test))

(struct target-file [name info] #:prefab)
(struct context [predecessor datum info] #:prefab)
(struct test [context datum info] #:prefab)

(define (printr x)
  (printf ": ~a~n" x)
  x)

(define (into-records in out)
  (define test-data (with-input-from-file in get-test-code))

  (define hash-start (hash 0 (target-file (~a in) '())))
  (define h (generate-records test-data hash-start 1 0))
  (with-output-to-file out (lambda () (pretty-write h)) #:exists 'replace))

(define (generate-records test-data h next-id pred-id)
  (cond
    [(null? test-data) h]
    [else
     (cond
       [(is-test? (first test-data))
        (define new-h (hash-set h next-id (test pred-id (first test-data) '())))
        (generate-records (rest test-data) new-h (add1 next-id) pred-id)]
       [else
        (define-values (ctxt rest-test-data) (consume-context test-data))
        (define new-h (hash-set h next-id (context pred-id (cons 'begin ctxt) '())))
        (generate-records rest-test-data new-h (add1 next-id) next-id)])]))

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
      (into-records src-file (path-replace-extension (build-path test-dir name) ".rktd"))
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
