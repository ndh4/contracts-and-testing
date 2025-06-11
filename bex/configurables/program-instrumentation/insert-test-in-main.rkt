#lang at-exp racket/base

(require racket/contract/base
         racket/match
         racket/file
         "../../util/program.rkt"
         "../../util/tests.rkt"
         "instrument-program.rkt")

(provide (contract-out [instrument-program instrument-program/c])
         test-to-add)

(define-logger insert-in-main)

(define recvr (make-log-receiver insert-in-main-logger 'debug))

#;(void (thread (lambda ()
                  (let loop ()
                    (define v (sync recvr))
                    (parameterize ([print-syntax-width +inf.0])
                      (printf "[~a] ~a~n" (vector-ref v 0) (vector-ref v 1)))
                    (loop)))))

(define test-to-add (make-parameter #f))

(define (get-test-to-add a-program)

  (log-insert-in-main-info (format "Inside GTTA, current-test-id is ~a" (current-test-id)))
  (and (current-test-id) (lookup-test (current-test-id) (mod-path (program-main a-program)))))

(define (instrument-program a-program make-instrumented-module)
  (match-define (program main-module other-modules-to-instrument) a-program)

  (log-insert-in-main-info "Hello??")

  (define main/instrumented

    (parameterize ([test-to-add (get-test-to-add a-program)])

      (make-instrumented-module main-module)))

  (log-insert-in-main-info "After")

  (define others/instrumented (map make-instrumented-module other-modules-to-instrument))

  (program main/instrumented others/instrumented))
