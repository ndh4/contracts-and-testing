#lang at-exp racket

(require syntax/parse
         syntax/parse/define
         (for-syntax syntax/parse)
         "../../util/program.rkt"
         "../../util/optional-contracts.rkt"
         "instrument-module.rkt"
         "../program-instrumentation/insert-test-in-main.rkt")

(provide (contract-out
          [instrument-module module-instrumenter/c]))

(define (instrument-module a-mod)

  (define tta (test-to-add))

  a-mod
  (if tta
      (add-test-to-module tta a-mod)
      a-mod))

(define (add-test-to-module test-to-add the-mod)
  (define instrumented-stx
    (add-test test-to-add
     (mod-stx the-mod)))
  (parameterize ([print-syntax-width +inf.0])
    (printf "Instrumented syntax is ~a~n" instrumented-stx))
  (struct-copy mod the-mod
               [stx instrumented-stx]))

(define (add-test test-to-add stx)
  (syntax-parse stx
    [(module name lang
                      (#%module-begin
                       body ...))
     #`(module name lang
         (#%module-begin
          body ...
          #,(datum->syntax #f test-to-add)
          ))]))
