#lang at-exp racket/base

(require racket/contract/base
         racket/match
         "../../util/program.rkt"
         "instrument-program.rkt")

(provide (contract-out [instrument-program instrument-program/c])
         test-to-add)

(define test-to-add (make-parameter #f))

(define (instrument-program a-program make-instrumented-module)
  (match-define (program main-module other-modules-to-instrument)
    a-program)

  (define main/instrumented
    
    (parameterize
      ([test-to-add '(begin
                       (require rackunit)
                       (define a 1)
                       (define b 2)
                       (define c 3)
                       (check-equal? (+ a b) c)
                       (define d -5)
                       (define e -6)
                       (check-equal? (+ d e) -11))])
      
    (make-instrumented-module main-module)))
  
  
  (define others/instrumented
    (map make-instrumented-module other-modules-to-instrument))
  
  (program main/instrumented others/instrumented))