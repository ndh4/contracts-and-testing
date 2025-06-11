#lang racket

(require "experiment-manager.rkt")

(define host local)

(printf (format-status host))
(pretty-print (summarize-experiment-status host))