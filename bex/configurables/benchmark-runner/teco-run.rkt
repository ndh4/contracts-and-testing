#lang at-exp racket

(require "benchmark-runner.rkt"
         "../../util/optional-contracts.rkt"
         racket/runtime-path
         db)

(provide (contract-out (rename make-require-benchmark-runner
                               make-benchmark-runner
                               make-benchmark-runner/c)))

(define-logger teco-run)

(define recvr (make-log-receiver teco-run-logger 'debug))

(void (thread (lambda ()
                (let loop ()
                  (define v (sync recvr))
                  (parameterize ([print-syntax-width +inf.0])
                    (printf "[~a] ~a~n" (vector-ref v 0) (vector-ref v 1)))
                  (loop)))))

(define (make-require-benchmark-runner program mod-name index)

  (λ (main-mod-path)
    ; Below is the code typically used for basic running
    ; Keep in mind that we don't have to worry about namespace because
    ; this is running within a parameterized environment.

    ; TODO: Notice how the code in instrumented-runner.rkt
    ;       goes in two levels and evaluates main submod if that's a
    ;       thing? Yeah, I think that our teco instrumentor needs to do
    ;       that same logic when adding tests.

    (log-teco-run-info "main-mod-path: ~a~n" (file-name-from-path (second main-mod-path)))
    (log-teco-run-info "program ~a~n" program)
    (log-teco-run-info "mod-name: ~a~n" mod-name)

    (define mod-res (eval `(require ,main-mod-path)))

    mod-res))
