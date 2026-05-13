#lang racket

(require "reduction-structs.rkt")

(provide test-check-configs
         slices-to-process)

(define/contract test-check-configs (listof (or/c 'yes_tc 'no_tc 'both_tc))
  (list 'yes_tc 'no_tc 'both_tc))

(define/contract slices-to-process
  (listof slice?)
  (list
      (slice "teco-04-20-2026@16:23:46" "kcfa" 0)
      (slice "teco-04-30-2026@11:13:36" "kcfa" 1111111)
      (slice "teco-04-20-2026@16:23:46" "kcfa" 2222222)
;      (slice "teco-04-21-2026@23:27:58" "morsecode" 0)
;      (slice "teco-04-30-2026@13:19:27" "morsecode" 1111)
;      (slice "teco-04-21-2026@23:27:58" "morsecode" 2222)
;      (slice "teco-04-25-2026@16:15:01" "forth" 0)
;      (slice "teco-05-01-2026@14:24:16" "forth" 1111)
;      (slice "teco-04-25-2026@16:15:01" "forth" 2222)
;      (slice "teco-04-22-2026@10:43:21" "sieve" 0)
;      (slice "teco-04-30-2026@17:02:02" "sieve" 11)
;      (slice "teco-04-22-2026@10:43:21" "sieve" 22)
;      (slice "teco-04-24-2026@20:59:51" "dungeon" 0)
;      (slice "teco-05-01-2026@09:51:43" "dungeon" 11111)
;      (slice "teco-04-24-2026@20:59:51" "dungeon" 22222)
;      (slice "teco-04-24-2026@13:59:38" "snake" 0)
;      (slice "teco-05-01-2026@05:14:36" "snake" 11111111)
;      (slice "teco-04-24-2026@13:59:38" "snake" 22222222)
;      (slice "teco-04-23-2026@15:08:11" "mbta" 0)
;      (slice "teco-04-30-2026@18:34:21" "mbta" 111111)
;      (slice "teco-04-23-2026@15:08:11" "mbta" 222222)
))