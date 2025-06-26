#lang at-exp racket

(require syntax/parse
         syntax/parse/define
         (for-syntax syntax/parse)
         "../../util/program.rkt"
         "../../util/optional-contracts.rkt"
         "instrument-module.rkt"
         "../program-instrumentation/insert-test-in-main.rkt")

(provide (contract-out [instrument-module module-instrumenter/c]))

(define-logger insert-test)

(define recvr (make-log-receiver insert-test-logger 'debug))

#;(void (thread (lambda ()
                  (let loop ()
                    (define v (sync recvr))
                    (parameterize ([print-syntax-width +inf.0])
                      (printf "[~a] ~a~n" (vector-ref v 0) (vector-ref v 1)))
                    (loop)))))

(define (instrument-module a-mod)

  (define tta (test-to-add))

  (log-insert-test-info (format "Inserting test ~a" tta))

  a-mod
  (if tta
      (add-test-to-module tta a-mod)
      a-mod))

(define (add-test-to-module test-to-add the-mod)
  (define instrumented-stx (add-test test-to-add (mod-stx the-mod)))
  (struct-copy mod the-mod [stx instrumented-stx]))

(define (add-test test-to-add stx)
  (syntax-parse stx
    [(module name lang
       (#%module-begin body ...))
     #`(module name lang
         (#%module-begin body ... #,(datum->syntax #f test-to-add)))]))
