#lang racket

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
  (define test-data (with-input-from-file in get-code))

  (define hash-start (hash 0 (target-file (~a in) '())))
  (define h (generate-records test-data hash-start 1))
  (with-output-to-file out (lambda () (pretty-write h)) #:exists 'replace))

(define (generate-records test-data h next-id)
  (define pred-id 0)
  (cond
    [(null? test-data) h]
    [else
     (cond
       [(is-test? (first test-data))
        (define new-h (hash-set h next-id (test pred-id (first test-data) '())))
        (generate-records (rest test-data) new-h (add1 next-id))]
       [else (generate-records (rest test-data) h next-id)])]))

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

(define (get-code)
  (read-language)
  (read-rest))

(define (read-rest)
  (define s (read))
  (if (eof-object? s)
      null
      (cons s (read-rest))))

(define (create-dir-if-not-exists! path)
  (unless (directory-exists? path)
    (make-directory path)))

(define (split-up-code src-dir test-dir nontest-dirs)
  (create-dir-if-not-exists! test-dir)
  (for ([dir nontest-dirs])
    (create-dir-if-not-exists! dir))

  (define src-files (find-files (lambda (path) (equal? (path-get-extension path) #".rkt")) src-dir))

  (for ([src-file src-files])
    (define name (file-name-from-path src-file))
    (when name
      (into-records src-file (path-replace-extension (build-path test-dir name) ".rktd"))
      (for ([dir nontest-dirs])
        (replace-word-in-file src-file (build-path dir name) "(check-" "#;(check-")))))

(define (replace-word-in-file input-file output-file old-word new-word)
  (with-input-from-file
   input-file
   (lambda ()
     (with-output-to-file output-file
                          #:exists 'replace
                          (lambda ()
                            (for ([line (in-lines)])
                              (displayln (string-replace line old-word new-word))))))))

(module+ main
  (require racket/cmdline)
  (define source-dir (make-parameter #f))
  (define test-dir (make-parameter #f))
  (define typed-dir (make-parameter #f))
  (define untyped-dir (make-parameter #f))
  (command-line #:once-each
                [("--source") path "Path to source directory. Mandatory." (source-dir path)]
                [("--test-dir") path "Path to test directory. Mandatory." (test-dir path)]
                [("--typed-dest-dir")
                 path
                 "Path to typed directory (may or may not exist). Mandatory."
                 (typed-dir path)]
                [("--untyped-dest-dir")
                 path
                 "Path to untyped directory (may or may not exist). Mandatory."
                 (untyped-dir path)])
  (split-up-code (source-dir) (test-dir) (list (untyped-dir) (typed-dir))))
