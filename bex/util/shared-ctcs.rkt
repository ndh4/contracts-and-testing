#lang racket

(provide test-type/c)

(define test-type/c
  (or/c "hand" "rand_max" "rand_types"))