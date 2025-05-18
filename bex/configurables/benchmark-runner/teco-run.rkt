#lang at-exp racket

(require "benchmark-runner.rkt"
         "../../util/optional-contracts.rkt")

(provide (contract-out [rename make-require-benchmark-runner
                               make-benchmark-runner
                               make-benchmark-runner/c]))

(define (make-require-benchmark-runner program mod-name index)

  (printf "Here we are in make-rbr~n")

  (λ (main-mod-path)
    ; Below is the code typically used for basic running
    ; Keep in mind that we don't have to worry about namespace because
    ; this is running within a parameterized environment.

    ; TODO: Notice how this code (copied from instrumented-runner.rkt)
    ;       goes in two levels and evaluates main submod if that's a
    ;       thing? Yeah, I think that our teco instrumentor needs to do
    ;       that same logic when adding tests.

    (printf "And now we're here in the result lambda~n")
    (define mod-res (eval `(require ,main-mod-path)))
    (define has-main-submod?
      (eval `(module-declared? '(submod ,main-mod-path main))))
    (if has-main-submod?
        (eval `(require (submod ,main-mod-path main)))
        mod-res)
    ))