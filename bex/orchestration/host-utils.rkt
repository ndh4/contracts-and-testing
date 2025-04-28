#lang at-exp rscript

(provide with-temp-file
         call-with-temp-file
         check-success
         bool->option)

(require syntax/parse/define
         "../util/option.rkt")

(define-simple-macro (with-temp-file name body ...)
  (call-with-temp-file (λ (name) body ...)))

(define (call-with-temp-file f)
  (define temp (make-temporary-file))
  (begin0 (f temp)
    (when (file-exists? temp) (delete-file temp))))

(define (check-success exit-code/bool)
  (match exit-code/bool
    [(or 0 #t) #t]
    [else absent]))

(define (bool->option v)
  (or v absent))