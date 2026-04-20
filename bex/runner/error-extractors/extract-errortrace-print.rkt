#lang at-exp racket

(require "../../util/optional-contracts.rkt"
         "blamed-location-extractor.rkt"
         racket/port
         errortrace)
(provide (contract-out
          [extract-errortrace-print (-> exn:fail? string?)]))

(define (extract-errortrace-print e)
  (call-with-output-string
    (lambda (port) (print-error-trace port e))))